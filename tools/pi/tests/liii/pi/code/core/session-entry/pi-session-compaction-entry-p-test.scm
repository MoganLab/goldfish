;; pi-session-compaction-entry-p
;; 会话条目 record 构造与字段访问函数。
;;
;; 语法
;; ----
;; (pi-session-compaction-entry-p ...)
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
(let ((e (pi-session-compaction-entry "c1" "m1" "ts" "sum" "m1" 100 '((k . 1)))))
  (check-true (pi-session-compaction-entry? e))
  (check (pi-session-compaction-entry-id e) => "c1")
  (check (pi-session-compaction-entry-parent-id e) => "m1")
  (check (pi-session-compaction-entry-timestamp e) => "ts")
  (check (pi-session-compaction-entry-summary e) => "sum")
  (check (pi-session-compaction-entry-first-kept-entry-id e) => "m1")
  (check (pi-session-compaction-entry-tokens-before e) => 100)
  (check (pi-session-compaction-entry-details e) => '((k . 1)))
) ;let

(check-report)
