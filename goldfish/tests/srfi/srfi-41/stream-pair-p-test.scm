;; stream-pair?
;; 判断对象是否为流对（非空流）。
;;
;; 语法
;; ----
;; (stream-pair? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean?
;; obj 是流对时为 #t。
;;
;; 说明
;; ----
;; SRFI 41：等价于 (and (stream? obj) (not (stream-null? obj)))。
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream-pair? stream-null) => #f)
(check (stream-pair? (stream-cons 1 stream-null)) => #t)
(check (stream-pair? "four") => #f)
(check (stream-pair? (stream 1 2 3)) => #t)

(check-report)
