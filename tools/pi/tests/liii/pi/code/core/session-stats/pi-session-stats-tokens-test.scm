(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-tokens
;; 返回会话的 token 分类统计 record。
;;
;; 语法
;; ----
;; (pi-session-stats-tokens stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; pi-session-token-stats
;; 输入、输出及缓存 token 统计。
;;
;; 示例
;; ----
;; (pi-session-token-stats-total (pi-session-stats-tokens stats)) => 100

(let* ((tokens (make-pi-session-token-stats 10 20 30 40))
       (stats (make-pi-session-stats #f #f #f 0 0 0 0 0 tokens 0)))
  (check (pi-session-stats-tokens stats) => tokens)
  (check (pi-session-token-stats-total (pi-session-stats-tokens stats)) => 100))

(check-report)
