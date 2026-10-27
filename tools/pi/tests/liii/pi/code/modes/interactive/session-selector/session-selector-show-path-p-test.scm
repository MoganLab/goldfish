(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-show-path?：选择器当前是否显示会话文件路径
;; 默认为 #f，Ctrl+P 切换

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
) ;define

(let ((selector (make-session-selector (list (info "a")) '()
                  (lambda (path) #f) (lambda () #f))))
  (check-false (session-selector-show-path? selector))
  (component-handle-input selector 'ctrl-p)
  (check-true (session-selector-show-path? selector))
  (component-handle-input selector 'ctrl-p)
  (check-false (session-selector-show-path? selector))
) ;let

(check-report)
