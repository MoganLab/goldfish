;; find-last-session-message-id
;; 查找会话文件中最后一条消息条目的 ID。
;;
;; 语法
;; ----
;; (find-last-session-message-id file-path)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; 返回值
;; ----
;; (or string #f)
;; 最后一条消息的 ID，文件不存在或没有消息时返回 #f。

(import (liii check)
  (liii path)
  (liii pi code core session-manager append)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-find-last-session-message-id-test.jsonl")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 文件不存在时返回 #f
(check (find-last-session-message-id tmp-file) => #f)

;; 2. 返回最后一条消息的 ID
(path-write-text
  tmp-file
  (string-append "{\"type\":\"message\",\"id\":\"m1\"}\n"
    "{\"type\":\"message\",\"id\":\"m2\"}\n"
  )
) ;path-write-text
(check (find-last-session-message-id tmp-file) => "m2")

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)
