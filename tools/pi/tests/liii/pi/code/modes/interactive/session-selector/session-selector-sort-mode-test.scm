(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-sort-mode：选择器当前的排序方式
;; 默认为 recent（最近活动序），Ctrl+S 在 recent 与 relevance（搜索相关度）间切换

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
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
