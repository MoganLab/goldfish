(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-context-token-estimate-usage-tokens
;; 返回最近有效 usage 中的 token 数量。
;;
;; 语法
;; ----
;; (pi-context-token-estimate-usage-tokens estimate)
;;
;; 参数
;; ----
;; estimate : pi-context-token-estimate
;; token 估算结果 record。
;;
;; 返回值
;; ----
;; integer
;; 助手消息报告的 usage token 数，无有效 usage 时为 0。
;;
;; 示例
;; ----
;; (pi-context-token-estimate-usage-tokens estimate) => 100

(check (pi-context-token-estimate-usage-tokens
         (make-pi-context-token-estimate 102 100 2)) => 100)

(check-report)
