;; history?
;; 判断给定对象是否为输入历史管理器实例。
;;
;; 语法
;; ----
;; (history? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为历史管理器实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor history)
) ;import

(check-set-mode! 'report-failed)

(let ((h (make-history)))
  (check-true (history? h))
) ;let

(check-false (history? "not-a-history"))
(check-false (history? 123))

(check-report)
