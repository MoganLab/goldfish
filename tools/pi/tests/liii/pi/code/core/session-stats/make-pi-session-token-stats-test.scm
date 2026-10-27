(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; make-pi-session-token-stats
;; 构造会话 token 分类统计 record。
;;
;; 语法
;; ----
;; (make-pi-session-token-stats input output cache-read cache-write)
;;
;; 参数
;; ----
;; input、output、cache-read、cache-write : integer
;; 各类 token 的非负累计数量。
;;
;; 返回值
;; ----
;; pi-session-token-stats
;; total 由四类 token 之和计算的统计 record。
;;
;; 示例
;; ----
;; (pi-session-token-stats-total
;;   (make-pi-session-token-stats 10 20 30 40)) => 100

(let ((tokens (make-pi-session-token-stats 3000 500 1200 100)))
  (check-true (pi-session-token-stats? tokens))
  (check (pi-session-token-stats-input tokens) => 3000)
  (check (pi-session-token-stats-output tokens) => 500)
  (check (pi-session-token-stats-cache-read tokens) => 1200)
  (check (pi-session-token-stats-cache-write tokens) => 100)
  (check (pi-session-token-stats-total tokens) => 4800))

(check-report)
