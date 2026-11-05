;; persist-initial-user-msg!
;; 将首轮用户消息追加落盘到会话文件。
;;
;; 语法
;; ----
;; (persist-initial-user-msg! parsed sdir sfile init-msgs)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数 alist；--continue 时用于定位恢复来源。
;; sdir : string | #f
;; 会话目录；#f 表示不恢复历史消息。
;; sfile : string
;; 会话文件路径。
;; init-msgs : list
;; 初始消息列表。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction session)
  (liii pi code core session-manager)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

(define SFILE
  (path->string (path-join (path-temp-dir) "pi-persist-initial-user-msg-test.jsonl"))
) ;define

(when (file-exists? SFILE)
  (path-unlink (path SFILE))
) ;when

(save-session-header! SFILE "persist-test" "2026-01-01T00:00:00.000Z" "/tmp")

;; 1. 首轮用户消息追加落盘
(persist-initial-user-msg! '() #f SFILE (list (pi-user-message "hello world")))

(let ((entries (pi-read-session-entries SFILE)))
  ;; 会话头 + 一条用户消息
  (check (length entries) => 2)
  (check (cdr (assoc "type" (cadr entries))) => "message")
  (check (cdr (assoc "role" (cdr (assoc "message" (cadr entries))))) => "user")
  (check-true (string-contains? (cdr (assoc "content" (cdr (assoc "message" (cadr entries)))))
                  "hello world")))

;; 2. 再次调用：按追加语义继续落盘
(persist-initial-user-msg! '() #f SFILE (list (pi-user-message "second message")))

(let ((entries (pi-read-session-entries SFILE)))
  (check (length entries) => 3)
  (check-true (string-contains? (cdr (assoc "content" (cdr (assoc "message" (caddr entries)))))
                  "second message")))

(check-report)
