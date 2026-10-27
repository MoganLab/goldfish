(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-context-token-estimate-tokens
;; 返回当前上下文的总 token 估算。
;;
;; 语法
;; ----
;; (pi-context-token-estimate-tokens estimate)
;;
;; 参数
;; ----
;; estimate : pi-context-token-estimate
;; token 估算结果 record。
;;
;; 返回值
;; ----
;; integer
;; usage 基准与后续消息估算之和。
;;
;; 示例
;; ----
;; (pi-context-token-estimate-tokens estimate) => 102

(check (pi-context-token-estimate-tokens (make-pi-context-token-estimate 102 100 2)) => 102)
(check (pi-context-token-estimate-tokens (make-pi-context-token-estimate 0 0 0)) => 0)

(check-report)
