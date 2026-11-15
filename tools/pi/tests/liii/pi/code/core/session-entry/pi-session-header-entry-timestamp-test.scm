;; pi-session-header-entry-timestamp
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-header-entry-timestamp ...)
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
  (check-true (pi-session-header-entry? e))
  (check (pi-session-header-entry-version e) => 3)
  (check (pi-session-header-entry-id e) => "h1")
  (check (pi-session-header-entry-timestamp e) => "ts")
  (check (pi-session-header-entry-cwd e) => "/cwd")
) ;let

(check-report)
