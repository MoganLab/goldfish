;; append-session-entry!
;; 追加一条会话条目到会话文件。
;;
;; 语法
;; ----
;; (append-session-entry! file-path entry-json-str)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; entry-json-str : string
;; 已序列化的 JSON 条目文本。
;;
;; 返回值
;; ----
;; 不返回
;; 直接向会话文件追加一行条目。

(import (liii check)
  (liii path)
  (liii string)
  (liii pi code core session-manager append)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-append-session-entry-test.jsonl")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 追加条目后可读回该条目
(append-session-entry! tmp-file "{\"type\":\"message\",\"id\":\"m1\"}")
(let ((lines (path-read-lines tmp-file)))
  (check (length lines) => 1)
  (check-true (string-contains? (car lines) "\"id\":\"m1\""))
) ;let*

;; 2. 连续追加保留原有条目
(append-session-entry! tmp-file "{\"type\":\"message\",\"id\":\"m2\"}")
(check (length (path-read-lines tmp-file)) => 2)

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)
