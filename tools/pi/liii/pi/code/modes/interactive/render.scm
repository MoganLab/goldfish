;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 渲染层：全屏（备用屏）整帧布局。
;;
;; 帧结构（自上而下）：
;;   [对话记录视口]  可滚动，页眉与消息卡片都在其中，follow 末尾
;;   [待发送队列卡片]
;;   [输入区]        busy 时上边框替换为 Working 动画行
;;   [斜杠命令菜单]
;;   [页脚]
;; 对齐上游 packages/coding-agent/src/modes/interactive/chat-viewport.ts：
;; transcript 占据剩余空间（grow），dock 固定在底部。
(define-library (liii pi code modes interactive render)
  (export pi-app-render! pi-app-frame-lines pi-app-transcript-lines
    pi-app-scroll-up! pi-app-scroll-down! pi-app-scroll-top!
    pi-app-scroll-bottom! pi-app-scroll-line-up! pi-app-scroll-line-down!
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive views)
    (liii pi code modes interactive views theme)
    (liii pi code modes interactive working)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive thinking-selector)
    (liii pi code modes interactive model-selector)
    (liii pi code modes interactive session-selector)
  ) ;import
  (begin

    ;; 编辑器可视行预算：不超过终端高度的 1/3，
    ;; 且必须给对话记录视口留出至少 1 行
    (define %pi-editor-chrome-lines 1)

    (define (%pi-editor-max-lines rows footer-lines menu-lines pending-lines)
      (let* ((rows (max 4 rows))
             (reserved (+ footer-lines menu-lines pending-lines %pi-editor-chrome-lines))
             (by-ratio (max 1 (quotient rows 3)))
             (by-space (max 1 (- rows reserved)))
            ) ;
        (max 1 (min by-ratio by-space))
      ) ;let*
    ) ;define

    ;; 拆分可能包含 \n 的字符串行，保证帧行与屏幕行一一对应
    (define (%pi-split-lines items)
      (append-map (lambda (item)
                    (if (string-contains? item "\n") (string-split item "\n") (list item))
                  ) ;lambda
        items
      ) ;append-map
    ) ;define

    ;; 未跟随末尾时，在视口底部叠加“跳到最新消息”提示行
    ;; （对齐上游 tui-alt-screen.ts 的 scrollToEndIndicator）
    (define (%pi-scroll-hint-line width)
      (apply-card-bg " ↓ Jump to latest message · Ctrl+End " width
        %bg-pending %fg-title
      ) ;apply-card-bg
    ) ;define

    ;; 计算整帧内容与光标位置：
    ;; 返回 (values frame-lines cursor)
    (define (pi-app-frame-lines app)
      (let* ((term (%pi-app-term app))
             (size (terminal-size term))
             (w (car size))
             (rows (cadr size))
             ;; pending 容器内容纯由队列状态派生：busy 时展示队首等待消息
             (q (pi-app-queue app))
             (pending-box (%pi-app-pending-box app))
             (ed (pi-app-editor app))
             (active-selector (pi-app-active-selector app))
             (input-component (or active-selector ed))
             (footer-lines (%pi-split-lines (component-render (%pi-app-footer app) w)))
             (menu (and (not active-selector) (pi-app-slash-command-menu app)))
             (menu-lines (if menu (%pi-split-lines (component-render menu w)) '()))
            ) ;
        (container-clear! pending-box)
        (when (pi-queue-busy? q)
          ;; 待处理区分组展示全部排队消息（对齐上游 updatePendingMessagesDisplay）：
          ;; steering 组 → follow-up 组 → 撤回提示行 → 原等待队列（旧语义兼容）
          (for-each
            (lambda (text)
              (container-add! pending-box
                (make-queued-card (string-append "Steering: " text))
              ) ;container-add!
            ) ;lambda
            (%pi-app-steering-pending app)
          ) ;for-each
          (for-each
            (lambda (text)
              (container-add! pending-box
                (make-queued-card (string-append "Follow-up: " text))
              ) ;container-add!
            ) ;lambda
            (%pi-app-follow-up-pending app)
          ) ;for-each
          (when (or (pair? (%pi-app-steering-pending app))
                  (pair? (%pi-app-follow-up-pending app))
                ) ;or
            (container-add! pending-box
              (make-queued-card "↳ ESC abort · Alt+Up edit queued messages")
            ) ;container-add!
          ) ;when
          (let ((items (pi-queue-items q)))
            (unless (null? items)
              (container-add! pending-box (make-queued-card (car items)))
            ) ;unless
          ) ;let
        ) ;when
        (let* ((pending-lines (%pi-split-lines (component-render pending-box w)))
               (ed-lines
                 (let* ((_
                          (unless active-selector
                            (editor-set-max-lines! ed
                              (%pi-editor-max-lines rows
                                (length footer-lines)
                                (length menu-lines)
                                (length pending-lines)
                              ) ;%pi-editor-max-lines
                            ) ;editor-set-max-lines!
                          ) ;unless
                        ) ;_
                        (raw (%pi-split-lines (component-render input-component w)))
                       ) ;
                   (if (and (not active-selector) (pi-queue-busy? q) (pair? raw) %pi-editor-top-border)
                     (cons (working-border-line (pi-app-working app)
                             w
                             (pi-thinking-border-color (pi-app-thinking-level app))
                           ) ;working-border-line
                       (cdr raw)
                     ) ;cons
                     raw
                   ) ;if
                 ) ;let*
               ) ;ed-lines
               (dock-lines (append pending-lines ed-lines menu-lines footer-lines))
               ;; 对话记录视口占据剩余空间，至少 1 行
               (transcript-height (max 1 (- rows (length dock-lines))))
               (transcript-lines
                 (let* ((visible (scroll-view-render-lines (pi-app-scroll-view app) w transcript-height)
                        ) ;visible
                        (padded
                          (append visible (make-list (max 0 (- transcript-height (length visible))) ""))
                        ) ;padded
                        (n (length padded))
                       ) ;
                   ;; 视口固定高度：内容不足时补空行，dock 始终贴在屏幕底部；
                   ;; 离开跟随位置时用提示行覆盖视口最后一行
                   (if
                     (and (not (scroll-view-follow? (pi-app-scroll-view app))) (> n 0))
                     (append (take padded (- n 1)) (list (%pi-scroll-hint-line w)))
                     padded
                   ) ;if
                 ) ;let*
               ) ;transcript-lines
               (frame (append transcript-lines dock-lines))
               (input-base (+ transcript-height (length pending-lines)))
               (cur-pos
                 (if active-selector
                   ;; 选择器类型分发：搜索行光标定位（thinking 与 model 同构）
                   (let ((pos (cond ((thinking-selector? active-selector)
                                     (thinking-selector-cursor-position active-selector)
                                    ) ;
                                    ((model-selector? active-selector)
                                     (model-selector-cursor-position active-selector)
                                    ) ;
                                    ((session-selector? active-selector)
                                     (session-selector-cursor-position active-selector)
                                    ) ;
                                    (else '(6 2))
                              ) ;cond
                         ) ;pos
                        ) ;
                     (cons (cadr pos) (+ input-base (car pos)))
                   ) ;let
                   (let* ((vp (editor-viewport ed w))
                          (above (caddr vp))
                          (pos (editor-display-position ed w))
                          (ed-y (+ input-base (if %pi-editor-top-border 1 0) (if (> above 0) 1 0)))
                         ) ;
                     (cons (cadr pos) (+ ed-y (car pos)))
                   ) ;let*
                 ) ;if
               ) ;cur-pos
              ) ;
          (values frame cur-pos)
        ) ;let*
      ) ;let*
    ) ;define

    ;; 退出 TUI 后打印回主屏的对话记录（完整历史，不含页眉横幅与输入区）。
    ;; 对齐上游 fullscreenExitOutput=transcript 行为：
    ;; 退出全屏界面后，终端上仍可回看本次会话的全部消息。
    ;; 只渲染消息卡片容器（%pi-app-chat），页眉与底部 dock 不计入历史。
    (define (pi-app-transcript-lines app)
      (let* ((term (%pi-app-term app))
             (w (car (terminal-size term)))
             (children (container-children (%pi-app-chat app)))
            ) ;
        (%pi-split-lines (append-map (lambda (child) (component-render child w)) children)
        ) ;%pi-split-lines
      ) ;let*
    ) ;define

    (define (pi-app-render! app)
      (let ((screen (pi-app-screen app)))
        (let-values (((frame cur-pos) (pi-app-frame-lines app)))
          (alt-screen-render! screen frame cur-pos)
        ) ;let-values
      ) ;let
    ) ;define

    ;; 全屏模式下的对话记录滚动：
    ;; page-up/page-down 整页滚动，ctrl+shift+up/down 单行滚动，
    ;; ctrl+shift+home/end 跳到记录顶部/末尾（末尾即恢复 follow）
    (define (pi-app-scroll-up! app)
      (scroll-view-page-up! (pi-app-scroll-view app))
    ) ;define

    (define (pi-app-scroll-down! app)
      (scroll-view-page-down! (pi-app-scroll-view app))
    ) ;define

    (define (pi-app-scroll-line-up! app)
      (scroll-view-scroll-by! (pi-app-scroll-view app) -1)
    ) ;define

    (define (pi-app-scroll-line-down! app)
      (scroll-view-scroll-by! (pi-app-scroll-view app) 1)
    ) ;define

    (define (pi-app-scroll-top! app)
      (scroll-view-to-top! (pi-app-scroll-view app))
    ) ;define

    (define (pi-app-scroll-bottom! app)
      (scroll-view-to-bottom! (pi-app-scroll-view app))
    ) ;define

  ) ;begin
) ;define-library
