(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-context-token-estimate-trailing-tokens
;; 返回最近有效 usage 之后消息的 token 估算。
;;
;; 语法
;; ----
;; (pi-context-token-estimate-trailing-tokens estimate)
;;
;; 参数
;; ----
;; estimate : pi-context-token-estimate
;; token 估算结果 record。
;;
;; 返回值
;; ----
;; integer
;; usage 基准之后尾随消息的估算 token 数。
;;
;; 示例
;; ----
;; (pi-context-token-estimate-trailing-tokens estimate) => 2

(check (pi-context-token-estimate-trailing-tokens
         (make-pi-context-token-estimate 102 100 2)) => 2)

(check-report)
