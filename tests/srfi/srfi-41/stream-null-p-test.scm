;; stream-null?
;; 判断对象是否为空流。
;;
;; 语法
;; ----
;; (stream-null? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;; 返回值
;; ----
;; boolean?
;; obj 是空流时为 #t；非流对象返回 #f。
;;
;; 说明
;; ----
;; SRFI 41：非流参数返回 #f 而不是报错。

;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream-null? stream-null) => #t)
(check (stream-null? (stream-cons 1 stream-null)) => #f)
(check (stream-null? "four") => #f)
(check (stream-null? (stream)) => #t)

(check-report)
