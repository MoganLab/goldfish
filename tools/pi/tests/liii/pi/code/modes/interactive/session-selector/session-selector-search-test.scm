(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui component)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-search：选择器的搜索输入框组件
;; 普通字符键进入搜索框并即时过滤会话列表

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (editor-text (session-selector-search selector)) => "")
  (component-handle-input selector "gold")
  (check (editor-text (session-selector-search selector)) => "gold")
) ;let

(check-report)
