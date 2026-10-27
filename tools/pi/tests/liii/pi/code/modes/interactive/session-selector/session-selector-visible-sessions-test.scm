(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-visible-sessions：选择器当前可见的会话摘要列表
;; 默认按最近活动序；输入搜索词后按模糊匹配过滤

(define (info path first)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message first)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text first))
) ;define

(let ((selector (make-session-selector
                  (list (info "a" "alpha") (info "b" "beta")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (map (lambda (s) (cdr (assoc 'path s)))
           (session-selector-visible-sessions selector))
    => '("a" "b"))
  (component-handle-input selector "alpha")
  (check (map (lambda (s) (cdr (assoc 'path s)))
           (session-selector-visible-sessions selector))
    => '("a"))
) ;let

(check-report)
