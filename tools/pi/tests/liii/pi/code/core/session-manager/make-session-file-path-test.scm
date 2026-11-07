;; make-session-file-path
;; 生成会话文件的完整路径。
;;
;; 语法
;; ----
;; (make-session-file-path session-dir timestamp-iso session-id)
;;
;; 参数
;; ----
;; session-dir : (or string path)
;; 会话目录。
;;
;; timestamp-iso : string
;; 会话创建时间戳。
;;
;; session-id : string
;; 会话 ID。
;;
;; 返回值
;; ----
;; string
;; 会话文件路径。

(import (liii check)
  (liii string)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; 1. 时间戳中的冒号与点被替换为连字符
(let ((p (make-session-file-path "/tmp/sessions" "2026-01-02T03:04:05.678" "sid-1")))
  (check-true (string? p))
  (check-true (string-ends? p "sid-1.jsonl"))
  (check-true (string-contains? p "2026-01-02T03-04-05-678"))
) ;let*

(check-report)
