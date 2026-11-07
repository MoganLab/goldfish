;; save-session-header!
;; 写入会话文件头条目。
;;
;; 语法
;; ----
;; (save-session-header! file-path session-id timestamp-iso cwd)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; session-id : string
;; 会话 ID。
;;
;; timestamp-iso : string
;; 会话创建时间戳。
;;
;; cwd : string
;; 会话工作目录。
;;
;; 返回值
;; ----
;; 不返回
;; 直接向会话文件写入头部条目。

(import (liii check)
  (liii path)
  (liii string)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-save-session-header-test.jsonl")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 写入的头部条目包含会话元信息
(save-session-header! tmp-file "sid-1" "2026-01-02T03:04:05Z" "/tmp/work")
(let ((lines (path-read-lines tmp-file)))
  (check (length lines) => 1)
  (check-true (string-contains? (car lines) "\"type\":\"session\""))
  (check-true (string-contains? (car lines) "\"id\":\"sid-1\""))
  (check-true (string-contains? (car lines) "\"cwd\""))
) ;let*

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)
