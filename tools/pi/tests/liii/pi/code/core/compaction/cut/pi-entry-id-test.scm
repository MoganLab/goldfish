;; pi-entry-id
;; 读取会话条目的 ID。
;;
;; 语法
;; ----
;; (pi-entry-id entry)
;;
;; 参数
;; ----
;; entry : alist
;; 会话条目字典。
;;
;; 返回值
;; ----
;; string 或 #f
;; 条目 ID，缺失时为 #f。

(import (liii check) (liii pi code core compaction cut))

(check-set-mode! 'report-failed)

;; 1. 读取条目 ID
(check (pi-entry-id '((id . "msg-1"))) => "msg-1")

;; 2. 缺失 ID 时返回 #f
(check (pi-entry-id '((type . "message"))) => #f)

(check-report)
