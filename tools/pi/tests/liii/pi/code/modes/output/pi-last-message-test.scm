;; pi-last-message
;; 返回消息列表中的最后一条消息。
;;
;; 语法
;; ----
;; (pi-last-message msgs)
;;
;; 参数
;; ----
;; msgs : list
;; 消息列表。
;;
;; 返回值
;; ----
;; any
;; 最后一条消息，空列表返回 #f。

(import (liii check)
  (liii pi code modes output)
) ;import

(check-set-mode! 'report-failed)

;; 1. 空列表返回 #f
(check (pi-last-message '()) => #f)

;; 2. 返回最后一条消息
(let ((msgs (list "first" "middle" "last")))
  (check (pi-last-message msgs) => "last")
) ;let*

(check-report)
