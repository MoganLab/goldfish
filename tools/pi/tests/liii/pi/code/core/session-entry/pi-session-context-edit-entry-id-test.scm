;; pi-session-context-edit-entry-id
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-context-edit-entry-id ...)
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
(let ((e (pi-session-context-edit-entry "e1" "c1" "ts" "m1" '(("content" . "new")))))
  (check-true (pi-session-context-edit-entry? e))
  (check (pi-session-context-edit-entry-id e) => "e1")
  (check (pi-session-context-edit-entry-parent-id e) => "c1")
  (check (pi-session-context-edit-entry-timestamp e) => "ts")
  (check (pi-session-context-edit-entry-target-id e) => "m1")
  (check (pi-session-context-edit-entry-replacement e) => '(("content" . "new")))
) ;let

(check-report)
