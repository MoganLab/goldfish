(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-sort-mode：选择器当前的排序方式
;; 默认为 recent（最近活动序），Ctrl+S 在 recent 与 relevance（搜索相关度）间切换

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (session-selector-sort-mode selector) => 'recent)
  (component-handle-input selector 'ctrl-s)
  (check (session-selector-sort-mode selector) => 'relevance)
  (component-handle-input selector 'ctrl-s)
  (check (session-selector-sort-mode selector) => 'recent)
) ;let

(check-report)
