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
  ) ;import
  (begin

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
               ;; busy 时编辑器上边框替换为 Working 动画行（行数不变）
               (ed-lines
                 (let ((raw (component-render ed w)))
                   (if (and (pi-queue-busy? q) (pair? raw) %pi-editor-top-border)
                     (cons (working-border-line (pi-app-working app)
                             w
                             (pi-thinking-border-color (pi-app-thinking-level app))
                           ) ;working-border-line
                       (cdr raw)
                     ) ;cons
                     raw
                   ) ;if
                 ) ;let
               ) ;ed-lines
               (footer-lines (component-render (%pi-app-footer app) w))
               (ed-cur (editor-cursor ed))
               (ed-y (+ (length doc-lines) (if %pi-editor-top-border 1 0)))
               (cur-pos (cons (editor-display-col ed) (+ ed-y (car ed-cur))))
              ) ;
          (screen-render! screen (append doc-lines ed-lines footer-lines) cur-pos)
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
