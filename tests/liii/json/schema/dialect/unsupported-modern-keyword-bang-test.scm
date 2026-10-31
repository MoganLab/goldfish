(import (liii check) (liii json schema dialect))

(check-set-mode! 'report-failed)

;; unsupported-modern-keyword!
;; 检查 Schema 是否使用了尚未支持的新版关键字。
;;
;; 语法
;; ----
;; (unsupported-modern-keyword! schema)
;;
;; 参数
;; ----
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; 返回值
;; ----
;; boolean
;; 当前实现始终返回 #f。
;;
;; 注意
;; ----
;; 目前库只实现 2020-12 语义，尚未支持的关键字会被静默忽略，
;; 该函数为后续版本预留了拒绝入口。
;;
;; 示例
;; ----
;; (unsupported-modern-keyword! '(("type" . "integer"))) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-false (unsupported-modern-keyword! #t))
(check-false (unsupported-modern-keyword! #f))
(check-false (unsupported-modern-keyword! '(("type" . "integer"))))
(check-false (unsupported-modern-keyword! '(("x-unknown" . #f))))

(check-report)
