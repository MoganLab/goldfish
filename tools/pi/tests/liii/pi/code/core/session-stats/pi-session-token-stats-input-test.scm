(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-token-stats-input
;; 返回普通输入 token 数量。
;;
;; 语法
;; ----
;; (pi-session-token-stats-input tokens)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; token 统计 record。
;;
;; 返回值
;; ----
;; integer
;; 普通输入 token 的累计数量。
;;
;; 示例
;; ----
;; (pi-session-token-stats-input tokens) => 3000

(check (pi-session-token-stats-input (make-pi-session-token-stats 3000 500 1200 100)) => 3000)
(check (pi-session-token-stats-input (make-pi-session-token-stats 0 0 0 0)) => 0)

(check-report)
