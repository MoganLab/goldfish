;; pi-entry-message
;; 读取会话条目中的消息对象。
;;
;; 语法
;; ----
;; (pi-entry-message entry)
;;
;; 参数
;; ----
;; entry : alist
;; 会话条目字典。
;;
;; 返回值
;; ----
;; (or alist #f)
;; 消息对象，非消息类型条目返回 #f。

(import (liii check)
  (liii pi code core compaction cut)
) ;import

(check-set-mode! 'report-failed)

;; 1. 消息类型条目返回消息对象
(let ((entry (list (cons "type" "message")
                 (cons "message" (list (cons "role" "user"))))
      )) ;
  (check (pi-entry-message entry) => (list (cons "role" "user")))
) ;let*

;; 2. 非消息类型条目返回 #f
(check (pi-entry-message '((type . "compaction"))) => #f)

(check-report)
