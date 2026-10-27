(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-token-stats-cache-read
;; 返回从缓存读取的 token 数量。
;;
;; 语法
;; ----
;; (pi-session-token-stats-cache-read tokens)
;;
;; 参数
;; ----
;; tokens : pi-session-token-stats
;; token 统计 record。
;;
;; 返回值
;; ----
;; integer
;; 缓存读取 token 的累计数量。
;;
;; 示例
;; ----
;; (pi-session-token-stats-cache-read tokens) => 1200

(check (pi-session-token-stats-cache-read (make-pi-session-token-stats 3000 500 1200 100)) => 1200)
(check (pi-session-token-stats-cache-read (make-pi-session-token-stats 0 0 0 0)) => 0)

(check-report)
