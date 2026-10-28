;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 渲染层：整体布局上屏（header/chat/pending → editor → footer），
;; busy 时编辑器上边框替换为 Working 动画行并定位硬件光标
(define-library (liii pi code modes interactive render)
  (export pi-app-render!)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi agent queue)
    (liii pi code modes interactive state)
    (liii pi code modes interactive views)
    (liii pi code modes interactive working)
    (liii pi code modes interactive thinking)
    (liii pi code modes interactive thinking-selector)
    (liii pi code modes interactive model-selector)
  ) ;import
  (begin

    ;; 编辑器可视行预算：至少 5 行，且不超过终端高度的 1/3；
    ;; chrome 扣除上下边框与可能出现的上下滚动提示（各 1 行）
    (define %pi-editor-min-lines 5)
    (define %pi-editor-chrome-lines 4)

    (define (%pi-editor-max-lines rows footer-lines menu-lines)
      (let* ((rows (max 1 rows))
             (by-ratio (quotient rows 3))
             (by-space (- rows %pi-editor-chrome-lines footer-lines menu-lines))
            ) ;
        (max %pi-editor-min-lines (min by-ratio by-space))
      ) ;let*
    ) ;define

    (define (pi-app-render! app)
      (let* ((screen (pi-app-screen app))
             (term (%pi-app-term app))
             (w (car (terminal-size term)))
             ;; pending 容器内容纯由队列状态派生：busy 时展示队首等待消息
             (q (pi-app-queue app))
             (pending-box (%pi-app-pending-box app))
            ) ;
        (container-clear! pending-box)
        (when (pi-queue-busy? q)
          (let ((items (pi-queue-items q)))
            (unless (null? items)
              (container-add! pending-box (make-queued-card (car items)))
            ) ;unless
          ) ;let
        ) ;when
        (let* ((raw-doc-lines (component-render (%pi-app-doc app) w))
               ;; 规范化拆分可能包含 \n 的字符串行，确保 ed-y 计算与 screen-render! 完全对齐
               (doc-lines
                 (append-map (lambda (item)
                               (if (string-contains? item "\n") (string-split item "\n") (list item))
                             ) ;lambda
                   raw-doc-lines
                 ) ;append-map
               ) ;doc-lines
               (ed (pi-app-editor app))
               (active-selector (pi-app-active-selector app))
               (input-component (or active-selector ed))
               (footer-lines (component-render (%pi-app-footer app) w))
               (menu (and (not active-selector) (pi-app-slash-command-menu app)))
               (menu-lines (if menu (component-render menu w) '()))
               ;; busy 时编辑器上边框替换为 Working 动画行（行数不变）
               (ed-lines
                 (let* ((_ (unless active-selector
                             (editor-set-max-lines! ed
                               (%pi-editor-max-lines (cadr (terminal-size term))
                                 (length footer-lines)
                                 (length menu-lines)
                               ) ;%pi-editor-max-lines
                             ) ;editor-set-max-lines!
                           ) ;unless
                       ) ;
                        (raw (component-render input-component w))
                        (editor-lines
                          (if (and (not active-selector) (pi-queue-busy? q) (pair? raw) %pi-editor-top-border)
                              (cons (working-border-line (pi-app-working app)
                                      w
                                      (pi-thinking-border-color (pi-app-thinking-level app))
                                    ) ;working-border-line
                                (cdr raw)
                              ) ;cons
                              raw
                            ) ;if
                          ) ;editor-lines
                       ) ;
                     (append editor-lines menu-lines)
                   ) ;let*
                 ) ;ed-lines
                 (cur-pos
                 (if active-selector
                   ;; 选择器类型分发：搜索行光标定位（thinking 与 model 同构）
                   (let ((pos (cond ((thinking-selector? active-selector)
                                     (thinking-selector-cursor-position active-selector)
                                    ) ;
                                    ((model-selector? active-selector)
                                     (model-selector-cursor-position active-selector)
                                    ) ;
                                    (else '(6 2))
                              ) ;cond
                         ) ;pos
                        ) ;
                     (cons (cadr pos) (+ (length doc-lines) (car pos)))
                   ) ;let
                    (let* ((ed-cur (editor-cursor ed))
                           (vp (editor-viewport ed))
                           (top (car vp))
                           (above (caddr vp))
                           (ed-y (+ (length doc-lines)
                                    (if %pi-editor-top-border 1 0)
                                    above
                                  ) ;+
                           ) ;
                          ) ;
                      (cons (editor-display-col ed) (+ ed-y (- (car ed-cur) top)))
                    ) ;let*
                 ) ;if
               ) ;cur-pos
              ) ;
          (screen-render! screen (append doc-lines ed-lines footer-lines) cur-pos)
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
