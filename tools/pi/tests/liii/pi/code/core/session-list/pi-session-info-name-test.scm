(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info-name
;; 返回可选的会话名称。
;;
;; 语法
;; ----
;; (pi-session-info-name info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string 或 #f
;; 已命名会话返回名称，否则返回 #f。
;;
;; 示例
;; ----
;; (pi-session-info-name info) => "Named session"

(let ((named (make-pi-session-info "/tmp/named.jsonl" "named" "/tmp/project"
               "Named session" "created" "modified" 0 "(no messages)" ""))
      (unnamed (make-pi-session-info "/tmp/unnamed.jsonl" "unnamed" "/tmp/project"
                 #f "created" "modified" 0 "(no messages)" "")))
  (check (pi-session-info-name named) => "Named session")
  (check-false (pi-session-info-name unnamed)))

(check-report)
