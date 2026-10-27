(import (scheme base)
  (liii check)
  (liii tui)
  (liii tui component)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-search：选择器的搜索输入框组件
;; 普通字符键进入搜索框并即时过滤会话列表

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (editor-text (session-selector-search selector)) => "")
  (component-handle-input selector "gold")
  (check (editor-text (session-selector-search selector)) => "gold")
) ;let

(check-report)
