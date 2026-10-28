(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report)

(define (paste-event text)
  (string-append "\x1b;[200~" text "\x1b;[201~")
) ;define

;; 大段粘贴在编辑器里显示标记，提交时还原为原文
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (submitted #f)
       (handler (lambda (app msg) (set! submitted msg)))
       (app (make-pi-app :terminal vterm :handler handler))
       (ed (pi-app-editor app))
       (big (string-append "a" (make-string 11 #\newline) "b"))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed (paste-event big))
  (check (editor-text ed) => "[paste #1 +12 lines]")
  (component-handle-input ed 'enter)
  (check submitted => big)
  (check (editor-text ed) => "")
) ;let*

;; 小段多行粘贴直接展开并原样提交
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (submitted #f)
       (handler (lambda (app msg) (set! submitted msg)))
       (app (make-pi-app :terminal vterm :handler handler))
       (ed (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed (paste-event "l1\nl2"))
  (check (editor-text ed) => "l1\nl2")
  (component-handle-input ed 'enter)
  (check submitted => "l1\nl2")
) ;let*

(check-report)
