(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; make-pi-session-info
;; 构造会话选择器使用的会话摘要 record。
;;
;; 语法
;; ----
;; (make-pi-session-info path id cwd name created modified message-count
;;                       first-message all-messages-text)
;;
;; 参数
;; ----
;; path : string
;; 会话 JSONL 文件路径。
;; id : string
;; 会话标识。
;; cwd : string
;; 会话工作目录。
;; name : string 或 #f
;; 可选会话名称。
;; created、modified : string
;; 创建和最近活动时间戳。
;; message-count : integer
;; 消息数量。
;; first-message、all-messages-text : string
;; 首条用户消息和全部消息投影文本。
;;
;; 返回值
;; ----
;; pi-session-info
;; 字段可通过对应访问器读取的会话摘要 record。
;;
;; 示例
;; ----
;; (pi-session-info-id
;;   (make-pi-session-info "s.jsonl" "id-1" "/work" #f "c" "m" 0 "" ""))
;; => "id-1"

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              "Demo" "2026-10-20T08:00:00.000Z" "2026-10-20T08:00:02.000Z"
              2 "first question" "first question first answer")))
  (check-true (pi-session-info? info))
  (check (pi-session-info-path info) => "/tmp/session.jsonl")
  (check (pi-session-info-id info) => "session-1")
  (check (pi-session-info-cwd info) => "/tmp/project")
  (check (pi-session-info-name info) => "Demo")
  (check (pi-session-info-created info) => "2026-10-20T08:00:00.000Z")
  (check (pi-session-info-modified info) => "2026-10-20T08:00:02.000Z")
  (check (pi-session-info-message-count info) => 2)
  (check (pi-session-info-first-message info) => "first question")
  (check (pi-session-info-all-messages-text info) => "first question first answer"))

(check-report)
