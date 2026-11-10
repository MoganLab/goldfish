;; get-session-name
;; 读取会话文件中记录的会话名称。
;;
;; 语法
;; ----
;; (get-session-name file-path)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; 返回值
;; ----
;; (or string #f)
;; 会话名称，文件不存在或没有名称记录时返回 #f。

(import (liii check) (liii path) (liii pi code core session-name))

(check-set-mode! 'report-failed)

(define tmp-file (path-join (path-temp-dir) "pi-get-session-name-test.jsonl"))

(when (path-exists? tmp-file)
  (path-unlink tmp-file)
) ;when

;; 1. 文件不存在时返回 #f
(check (get-session-name (path->string tmp-file)) => #f)

;; 2. 读取最后一条会话名称记录
(path-write-text tmp-file
  (string-append "{\"type\":\"session_info\",\"name\":\"first\"}\n"
    "{\"type\":\"session_info\",\"name\":\"latest\"}\n"
  ) ;string-append
) ;path-write-text
(check (get-session-name (path->string tmp-file)) => "latest")

(when (path-exists? tmp-file)
  (path-unlink tmp-file)
) ;when

(check-report)
