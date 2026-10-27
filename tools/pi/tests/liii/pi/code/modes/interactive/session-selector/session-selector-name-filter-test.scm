(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-name-filter：选择器当前的命名过滤状态
;; 默认为 all（全部会话），Ctrl+N 在 all 与 named（仅命名会话）间切换

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (session-selector-name-filter selector) => 'all)
  (component-handle-input selector 'ctrl-n)
  (check (session-selector-name-filter selector) => 'named)
  (component-handle-input selector 'ctrl-n)
  (check (session-selector-name-filter selector) => 'all)
) ;let

(check-report)
