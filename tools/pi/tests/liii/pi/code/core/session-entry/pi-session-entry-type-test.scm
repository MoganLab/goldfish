;; pi-session-entry-type
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-entry-type ...)
;;
;; 参数
;; ----
;; entry : any?
;; 会话条目 record 或构造参数。
;;
;; 返回值
;; ----
;; any?
;; 对应的会话条目 record、谓词布尔值或字段值。

(import (liii check) (liii pi code core session-entry))
(check-set-mode! 'report-failed)
(let ((e (pi-session-header-entry "h1" "ts" "/cwd")))
  (check-true (pi-session-entry? e))
  (check-false (pi-session-entry? "not-entry"))
  (check (pi-session-entry-type e) => "session")
  (check (pi-session-entry-id e) => "h1")
  (check-false (pi-session-entry-parent-id e))
  (check (pi-session-entry-timestamp e) => "ts")
) ;let

(check-report)
