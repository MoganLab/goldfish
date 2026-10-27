(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector?：判断对象是否为会话选择器组件

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check-true (session-selector? selector))
  (check-false (session-selector? '()))
  (check-false (session-selector? "not a selector"))
) ;let

(check-report)
