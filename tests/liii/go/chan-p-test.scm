(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; chan?
;; 判断对象是否为通道。
;;
;; 语法
;; ----
;; (chan? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是通道返回 #t，否则返回 #f。

(define ch (make-chan))
(check (chan? ch) => #t)
(check (chan? 123) => #f)
(check (chan? "channel") => #f)
(check (chan? '()) => #f)

(check-report)
