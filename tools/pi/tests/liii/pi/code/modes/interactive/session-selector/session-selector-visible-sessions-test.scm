(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-visible-sessions：选择器当前可见的会话摘要列表
;; 默认按最近活动序；输入搜索词后按模糊匹配过滤

(define (info path first)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 first first)
) ;define

(let ((selector (make-session-selector
                  (list (info "a" "alpha") (info "b" "beta")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (map pi-session-info-path (session-selector-visible-sessions selector))
    => '("a" "b"))
  (component-handle-input selector "alpha")
  (check (map pi-session-info-path (session-selector-visible-sessions selector))
    => '("a"))
) ;let

(check-report)
