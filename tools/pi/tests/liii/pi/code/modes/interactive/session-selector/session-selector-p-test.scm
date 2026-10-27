(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector?：判断对象是否为会话选择器组件

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check-true (session-selector? selector))
  (check-false (session-selector? '()))
  (check-false (session-selector? "not a selector"))
) ;let

(check-report)
