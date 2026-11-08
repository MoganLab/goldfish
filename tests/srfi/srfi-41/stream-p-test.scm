;; stream?
;; 判断对象是否为流。
;;
;; 语法
;; ----
;; (stream? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;; 返回值
;; ----
;; boolean?
;; obj 是流时为 #t。
;;
;; 说明
;; ----
;; SRFI 41：流包括空流与流对。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream? stream-null) => #t)
(check (stream? (stream-cons 1 stream-null)) => #t)
(check (stream? "four") => #f)
(check (stream? '()) => #f)
(check (stream? (stream 1 2 3)) => #t)

(check-report)
