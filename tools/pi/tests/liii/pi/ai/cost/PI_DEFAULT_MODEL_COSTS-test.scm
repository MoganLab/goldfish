;; PI_DEFAULT_MODEL_COSTS
;; 默认模型费率定价表。
;;
;; 语法
;; ----
;; PI_DEFAULT_MODEL_COSTS
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 各服务商内置模型的费率关联列表。

(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

(check-true (pair? PI_DEFAULT_MODEL_COSTS))
(check-true (pair? (assoc "google" PI_DEFAULT_MODEL_COSTS)))
(check-true (pair? (assoc "anthropic" PI_DEFAULT_MODEL_COSTS)))

(check-report)
