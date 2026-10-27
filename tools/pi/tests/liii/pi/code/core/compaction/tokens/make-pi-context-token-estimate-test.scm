(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; make-pi-context-token-estimate
;; 构造上下文 token 估算结果 record。
;;
;; 语法
;; ----
;; (make-pi-context-token-estimate tokens usage-tokens trailing-tokens)
;;
;; 参数
;; ----
;; tokens : integer
;; 当前上下文的总 token 估算。
;; usage-tokens : integer
;; 最近一条有效助手消息报告的 token 数量。
;; trailing-tokens : integer
;; 该助手消息之后消息的估算 token 数量。
;;
;; 返回值
;; ----
;; pi-context-token-estimate
;; token 估算结果 record。
;;
;; 示例
;; ----
;; (pi-context-token-estimate-tokens
;;   (make-pi-context-token-estimate 102 100 2)) => 102

(let ((estimate (make-pi-context-token-estimate 102 100 2)))
  (check-true (pi-context-token-estimate? estimate))
  (check (pi-context-token-estimate-tokens estimate) => 102)
  (check (pi-context-token-estimate-usage-tokens estimate) => 100)
  (check (pi-context-token-estimate-trailing-tokens estimate) => 2))

(check-report)
