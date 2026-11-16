;; pi-session-state-entry-type
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-state-entry-type ...)
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

(import (liii check) (liii pi code core session-entry state))
(check-set-mode! 'report-failed)
(let ((e (pi-session-state-entry "model_change" "s1" "e1" "ts" '(("provider" . "google")))))
  (check-true (pi-session-state-entry? e))
  (check (pi-session-state-entry-type e) => "model_change")
  (check (pi-session-state-entry-id e) => "s1")
  (check (pi-session-state-entry-parent-id e) => "e1")
  (check (pi-session-state-entry-timestamp e) => "ts")
  (check (pi-session-state-entry-fields e) => '(("provider" . "google")))
) ;let

(check-report)
