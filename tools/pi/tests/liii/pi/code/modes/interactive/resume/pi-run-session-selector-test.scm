(import (scheme base)
  (liii check)
  (liii path)
  (liii tui)
  (liii pi code modes interactive resume)
) ;import

(check-set-mode! 'report-failed)

;; pi-run-session-selector：运行会话选择器 TUI
;; 返回选中会话的文件路径；Escape / Ctrl+C 取消时返回 #f

(define (info path)
  (list (cons 'path path) (cons 'id path) (cons 'cwd "/work")
    (cons 'name #f) (cons 'first-message path)
    (cons 'modified "2026-10-21T08:00:01.000Z")
    (cons 'message-count 1) (cons 'all-messages-text path))
) ;define

;; 1. down + enter 选中第二条
(let ((vt (make-virtual-terminal :width 100 :height 30)))
  (virtual-terminal-push-key! vt 'down)
  (virtual-terminal-push-key! vt 'enter)
  (check (pi-run-session-selector (list (info "new") (info "old")) '() :terminal vt)
    => "old")
) ;let

;; 2. escape 取消返回 #f
(let ((vt (make-virtual-terminal :width 100 :height 30)))
  (virtual-terminal-push-key! vt 'escape)
  (check (pi-run-session-selector (list (info "new")) '() :terminal vt) => #f)
) ;let

(check-report)
