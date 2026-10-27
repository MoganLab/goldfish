(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-token-stats-cache-write
;; 返回写入缓存的 token 数量。
;;
;; 语法
;; ----
;; (pi-session-token-stats-cache-write tokens)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; token 统计 record。
;;
;; 返回值
;; ----
;; integer
;; 缓存写入 token 的累计数量。
;;
;; 示例
;; ----
;; (pi-session-token-stats-cache-write tokens) => 100

(check (pi-session-token-stats-cache-write (make-pi-session-token-stats 3000 500 1200 100)) => 100)
(check (pi-session-token-stats-cache-write (make-pi-session-token-stats 0 0 0 0)) => 0)

(check-report)
