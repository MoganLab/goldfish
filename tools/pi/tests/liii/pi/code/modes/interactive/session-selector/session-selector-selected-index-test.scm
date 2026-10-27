(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-selected-index：选择器当前选中项的下标
;; 默认 0；up/down/page-up/page-down 移动并在边界处钳制

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
) ;define

(let ((selector (make-session-selector
                  (list (info "a") (info "b") (info "c")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check (session-selector-selected-index selector) => 0)
  (component-handle-input selector 'down)
  (check (session-selector-selected-index selector) => 1)
  (component-handle-input selector 'page-down)
  (check (session-selector-selected-index selector) => 2)
  (component-handle-input selector 'up)
  (check (session-selector-selected-index selector) => 1)
  (component-handle-input selector 'page-up)
  (check (session-selector-selected-index selector) => 0)
) ;let

(check-report)
