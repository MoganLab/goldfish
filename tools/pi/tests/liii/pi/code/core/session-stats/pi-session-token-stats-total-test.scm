(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-token-stats-total
;; 计算四类 token 数量的总和。
;;
;; 语法
;; ----
;; (pi-session-token-stats-total tokens)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; token 统计 record。
;;
;; 返回值
;; ----
;; integer
;; input、output、cache-read 与 cache-write 之和。
;;
;; 示例
;; ----
;; (pi-session-token-stats-total tokens) => 4800

(check (pi-session-token-stats-total (make-pi-session-token-stats 3000 500 1200 100)) => 4800)
(check (pi-session-token-stats-total (make-pi-session-token-stats 0 0 0 0)) => 0)

(check-report)
