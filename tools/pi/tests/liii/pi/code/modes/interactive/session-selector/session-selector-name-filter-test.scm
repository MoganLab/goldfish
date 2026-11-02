;; session-selector-name-filter
;; session-selector-name-filter 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (session-selector-name-filter ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core session-list)
  (liii pi code modes interactive session-selector)
) ;import

(check-set-mode! 'report-failed)

;; session-selector-name-filter：选择器当前的命名过滤状态
;; 默认为 all（全部会话），Ctrl+N 在 all 与 named（仅命名会话）间切换

(define (info path)
  (make-pi-session-info path path "/work" #f "created"
    "2026-10-21T08:00:01.000Z" 1 path path)
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
