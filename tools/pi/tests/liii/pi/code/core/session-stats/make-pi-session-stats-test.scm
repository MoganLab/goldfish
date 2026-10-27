(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; make-pi-session-stats
;; 构造会话消息、token 与费用的完整统计 record。
;;
;; 语法
;; ----
;; (make-pi-session-stats session-file session-id session-name total-messages
;;                        user-messages assistant-messages tool-calls tool-results
;;                        tokens cost)
;;
;; 参数
;; ----
;; session-file : string 或 #f
;; 会话文件路径，内存会话为 #f。
;; session-id、session-name : string 或 #f
;; 可选会话标识与名称。
;; total-messages、user-messages、assistant-messages : integer
;; 各类消息数量。
;; tool-calls、tool-results : integer
;; 工具调用与结果数量。
;; tokens : pi-session-token-stats
;; token 分类统计。
;; cost : number
;; 会话累计费用。
;;
;; 返回值
;; ----
;; pi-session-stats
;; 完整的会话统计 record。
;;
;; 示例
;; ----
;; (pi-session-stats-total-messages stats) => 5

(let* ((tokens (make-pi-session-token-stats 3000 500 1200 100))
       (stats (make-pi-session-stats "/tmp/session.jsonl" "session-1" "Demo"
                5 1 2 2 1 tokens 0.5)))
  (check-true (pi-session-stats? stats))
  (check (pi-session-stats-session-file stats) => "/tmp/session.jsonl")
  (check (pi-session-stats-session-id stats) => "session-1")
  (check (pi-session-stats-session-name stats) => "Demo")
  (check (pi-session-stats-total-messages stats) => 5)
  (check (pi-session-stats-user-messages stats) => 1)
  (check (pi-session-stats-assistant-messages stats) => 2)
  (check (pi-session-stats-tool-calls stats) => 2)
  (check (pi-session-stats-tool-results stats) => 1)
  (check (pi-session-stats-tokens stats) => tokens)
  (check (pi-session-stats-cost stats) => 0.5))

(check-report)
