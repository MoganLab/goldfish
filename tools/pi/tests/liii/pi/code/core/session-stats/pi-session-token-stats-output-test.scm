;; pi-session-token-stats-output
;; 返回输出 token 数量。
;;
;; 语法
;; ----
;; (pi-session-token-stats-output tokens)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; token 统计 record。
;;
;; 返回值
;; ----
;; integer
;; 输出 token 的累计数量。
;;

(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

(check (pi-session-token-stats-output (make-pi-session-token-stats 3000 500 1200 100)) => 500)
(check (pi-session-token-stats-output (make-pi-session-token-stats 0 0 0 0)) => 0)

(check-report)
