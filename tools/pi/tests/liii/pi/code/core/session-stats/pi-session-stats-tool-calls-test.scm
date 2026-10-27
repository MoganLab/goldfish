(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats-tool-calls
;; 返回助手消息中的工具调用数量。
;;
;; 语法
;; ----
;; (pi-session-stats-tool-calls stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; integer
;; toolCall 内容块的总数量。
;;
;; 示例
;; ----
;; (pi-session-stats-tool-calls stats) => 2

(let ((stats (make-pi-session-stats #f #f #f 2 0 2 2 0
               (make-pi-session-token-stats 0 0 0 0) 0)))
  (check (pi-session-stats-tool-calls stats) => 2))

(check-report)
