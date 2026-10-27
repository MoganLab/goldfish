(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

(define (paste-event text)
  (string-append "\x1b;[200~" text "\x1b;[201~")
) ;define

;; 1. gf pi 输入框：大段粘贴折叠为标记，提交时把完整内容交给 agent
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (received #f)
       (handler (lambda (app msg) (set! received msg)))
       (app (make-pi-app :terminal vterm :handler handler))
       (editor (pi-app-editor app))
       (big (make-string 1965 #\a))
      ) ;
  (pi-app-render! app)
  (pi-app-handle-input! app (paste-event big))
  (check (editor-text editor) => "[paste #1 1965 chars]")
  (pi-app-render! app)
  (let ((plain (map strip-ansi (screen-lines (pi-app-screen app)))))
    (check-true (any (lambda (line) (string-contains? line "[paste #1 1965 chars]")) plain)
    ) ;check-true
  ) ;let
  (component-handle-input editor 'enter)
  (check received => big)
  (check (editor-text editor) => "")
  ;; 提交后重新粘贴，编号从 #1 重新开始
  (pi-app-handle-input! app (paste-event big))
  (check (editor-text editor) => "[paste #1 1965 chars]")
) ;let*

;; 2. 小段多行粘贴直接进入编辑器，不触发提交
(let* ((vterm (make-virtual-terminal :width 100 :height 40))
       (received #f)
       (handler (lambda (app msg) (set! received msg)))
       (app (make-pi-app :terminal vterm :handler handler))
       (editor (pi-app-editor app))
      ) ;
  (pi-app-render! app)
  (pi-app-handle-input! app (paste-event "line1\nline2"))
  (check (editor-text editor) => "line1\nline2")
  (check received => #f)
  (component-handle-input editor 'enter)
  (check received => "line1\nline2")
) ;let*

(check-report)
