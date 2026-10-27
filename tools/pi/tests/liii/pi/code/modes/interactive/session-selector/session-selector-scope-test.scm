(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-scope：选择器当前的会话范围
;; 默认为 current（当前目录），Tab 键在 current 与 all（全部项目）间切换

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((selector (make-session-selector (list (info "a")) (list (info "a") (info "b"))
                  (lambda (path) #f) (lambda () #f))))
  (check (session-selector-scope selector) => 'current)
  (component-handle-input selector 'tab)
  (check (session-selector-scope selector) => 'all)
  (component-handle-input selector 'tab)
  (check (session-selector-scope selector) => 'current)
) ;let

(check-report)
