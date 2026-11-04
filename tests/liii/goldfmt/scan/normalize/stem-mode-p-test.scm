;; stem-mode?
;; 判断当前是否处于 stem 模式。
;;
;; 语法
;; ----
;; (stem-mode?)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; boolean?
;; 处于 stem 模式时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; stem 模式由 `call-with-stem-mode` 临时开启，
;; 在该模式下 quote 形式的 datum 不会被折叠为特殊的 env 节点。

(import (liii check) (liii goldfmt scan normalize))

(check-set-mode! 'report-failed)

;; 默认不在 stem 模式
(check (stem-mode?) => #f)

;; 在 stem 模式内返回 #t，退出后恢复为 #f
(check (call-with-stem-mode (lambda () (stem-mode?))) => #t)
(check (stem-mode?) => #f)

(check-report)
