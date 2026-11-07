;; save-session-message!
;; 写入一条会话消息条目。
;;
;; 语法
;; ----
;; (save-session-message! file-path message-json-str id parent-id timestamp-iso)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; message-json-str : string
;; 已序列化的消息 JSON 文本。
;;
;; id : string
;; 消息条目的 ID。
;;
;; parent-id : (or string #f)
;; 父条目 ID，根消息为 #f。
;;
;; timestamp-iso : string
;; 消息时间戳。
;;
;; 返回值
;; ----
;; 不返回
;; 直接向会话文件写入消息条目。

(import (liii check)
  (liii path)
  (liii string)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-save-session-message-test.jsonl")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 写入的消息条目包含 ID 与父条目 ID
(save-session-message! tmp-file "{\"role\":\"user\",\"content\":\"hi\"}" "m1" "sess-1" "2026-01-02T03:04:05Z")
(let ((lines (path-read-lines tmp-file)))
  (check (length lines) => 1)
  (check-true (string-contains? (car lines) "\"type\":\"message\""))
  (check-true (string-contains? (car lines) "\"id\":\"m1\""))
  (check-true (string-contains? (car lines) "\"parentId\":\"sess-1\""))
) ;let*

;; 2. 父条目为 #f 时写入 null
(save-session-message! tmp-file "{\"role\":\"user\",\"content\":\"hi\"}" "m2" #f "2026-01-02T03:04:06Z")
(let ((lines (path-read-lines tmp-file)))
  (check (length lines) => 2)
  (check-true (string-contains? (cadr lines) "\"parentId\":null"))
) ;let*

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)
