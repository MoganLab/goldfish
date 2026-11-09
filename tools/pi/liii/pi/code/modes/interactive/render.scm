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
    %pi-app-working-border-row %pi-app-render-working-border!
    %pi-app-handle-resize! pi-app-scroll-up! pi-app-scroll-down!
    pi-app-scroll-top! pi-app-scroll-bottom! pi-app-scroll-line-up!
    pi-app-scroll-line-down!
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
    (liii os)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive selection)
    (liii pi code modes interactive views)
    (liii pi code modes interactive views theme)
    (liii pi code modes interactive working)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive thinking-selector)
    (liii pi code modes interactive model-selector)
    (liii pi code modes interactive session-selector)
    (liii pi code modes interactive common)
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

    ;; 在第 0 行右上角合成瞬态浮层提示（对齐上游 compositeFlashes）
    (define (%composite-flash-line line flash-text width)
      (let* ((label (string-append " " flash-text " ")) (label-w (display-width label)))
        (if (or (<= width 0) (>= label-w width))
          line
          (let* ((start-col (- width label-w))
                 (overlay (string-append "\x1b;[7m" label "\x1b;[27m"))
                 (base-cut (truncate-to-width line start-col ""))
                 (cut-w (display-width base-cut))
                 (pad (make-string (max 0 (- start-col cut-w)) #\space))
                ) ;
            (string-append base-cut pad "\x1b;[0m" overlay "\x1b;[0m")
          ) ;let*
        ) ;if
      ) ;let*
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
            (container-add! pending-box (make-queued-card (pi-pending-hint-line)))
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
               (rendered-frame
                 (let* ((sel-frame (if (pi-app-selection-active? app)
                                     (highlight-selection-lines frame (pi-app-selection app))
                                     frame
                                   ) ;if
                        ) ;sel-frame
                        (flash-msg (pi-app-flash-message app))
                       ) ;
                   (if (and flash-msg (pair? sel-frame))
                     (cons (%composite-flash-line (car sel-frame) flash-msg w) (cdr sel-frame))
                     sel-frame
                   ) ;if
                 ) ;let*
               ) ;rendered-frame
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
          ;; 记录 dock 长度：供轻量 tick 推导 Working 边框行
          (%pi-app-dock-length-set! app (length dock-lines))
          (values rendered-frame cur-pos)
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

    ;; 在缓存帧中定位 Working 边框行（编辑器上边框，busy 时承载 spinner）。
    ;; 行号推导（确定性，无内容猜测）：
    ;;   dock 布局 = pending + editor + menu + footer（footer 最后，菜单在 editor 之后）
    ;;   边框行是 editor 区域的首行，其行号 = 帧长 - dock 长 + pending 行数。
    ;;   pending 行数 = busy 时各排队卡片 + 可选 ESC 提示行 + 可选队首卡片。
    ;; 返回 #f 表示缓存无效，调用方应回退整帧渲染。
    (define (%pi-app-working-border-row app)
      (let ((cached (pi-app-rendered-frame app)) (dock (pi-app-dock-length app)))
        (if (or (not cached) (not dock))
          #f
          (let* ((frame (car cached))
                 (n (length frame))
                 (q (pi-app-queue app))
                 (steer (%pi-app-steering-pending app))
                 (fu (%pi-app-follow-up-pending app))
                 (items (pi-queue-items q))
                 (pending-len
                   (+ (length steer)
                     (length fu)
                     (if (or (pair? steer) (pair? fu)) 1 0)
                     (if (pair? items) 1 0)
                   ) ;+
                 ) ;pending-len
                 (border-y (+ (- n dock) pending-len))
                ) ;
            (and (>= border-y 0) (< border-y n) border-y)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 终端缩放检测：主循环每轮开头调用。resize 后终端会对备用屏内容做
    ;; reflow，增量 diff 无法再保证屏幕干净；而空闲轮询里没有任何事件触发
    ;; 渲染，界面会停留在旧布局。尺寸状态由 <alt-screen> 记录自身维护，
    ;; 检测到变化时库内已整屏失效，这里只需按新尺寸重新布局重绘。
    (define (%pi-app-handle-resize! app)
      (when (alt-screen-resize-check! (pi-app-screen app))
        (pi-app-render! app)
      ) ;when
    ) ;define

    (define (pi-app-render! app)
      (let ((screen (pi-app-screen app)))
        (let-values (((frame cur-pos) (pi-app-frame-lines app)))
          (alt-screen-render! screen frame cur-pos)
          ;; 缓存归一化后的帧（与 buffer 上帧同源）：
          ;; Working 动画轻量 tick 基于此构造新帧，保证逐行 eq? 命中快速路径
          (%pi-app-rendered-frame-set! app (cons (alt-screen-lines screen) cur-pos))
        ) ;let-values
      ) ;let
    ) ;define

    ;; 轻量 tick 渲染：仅替换缓存帧中的 Working 边框行，不重算整帧布局。
    ;; busy 期间布局不变（每拍仅 spinner 帧变化），逐行 eq? 命中快速路径，
    ;; 避免每拍重算整帧（长会话整帧重算成本可达 ~16ms）。
    ;; 选择器激活时编辑器行被选择器替换，行定位不适用，返回 #f 由调用方回退整帧渲染。
    ;; 返回 #t 表示已按轻量路径渲染；#f 表示不可用（调用方应回退整帧渲染）。
    (define (%pi-app-render-working-border! app)
      (if (pi-app-active-selector app)
        #f
        (let ((row (%pi-app-working-border-row app)) (cached (pi-app-rendered-frame app)))
          (if (or (not row) (not cached))
            #f
            (%render-working-border! app row (car cached) (cdr cached))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 用新的 Working 边框行替换缓存帧的对应行，并提交到备用屏
    (define (%render-working-border! app row frame cursor)
      (let* ((w (car (terminal-size (pi-app-term app))))
             (color (pi-thinking-border-color (pi-app-thinking-level app)))
             (new-line (working-border-line (pi-app-working app) w color))
             (new-frame (append (take frame row) (list new-line) (drop frame (+ row 1))))
             (screen (pi-app-screen app))
            ) ;
        (alt-screen-render! screen new-frame cursor)
        ;; 缓存归一化后的帧（与 buffer 上帧同源），供下一拍
        (%pi-app-rendered-frame-set! app (cons (alt-screen-lines screen) cursor))
        #t
      ) ;let*
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
