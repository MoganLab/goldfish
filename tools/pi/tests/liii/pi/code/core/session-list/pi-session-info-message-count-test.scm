(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-message-count
;; 返回会话中的消息数量。
;;
;; 语法
;; ----
;; (pi-session-info-message-count info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; integer
;; 非负消息数量。
;;
;; 示例
;; ----
;; (pi-session-info-message-count info) => 12

(let ((empty (make-pi-session-info "/tmp/empty.jsonl" "empty" "/tmp/project"
               #f "created" "modified" 0 "(no messages)" ""))
      (active (make-pi-session-info "/tmp/active.jsonl" "active" "/tmp/project"
                #f "created" "modified" 12 "question" "question answer")))
  (check (pi-session-info-message-count empty) => 0)
  (check (pi-session-info-message-count active) => 12))

(check-report)
