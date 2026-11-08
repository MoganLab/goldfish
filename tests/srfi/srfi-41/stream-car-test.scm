;; stream-car
;; 返回流的第一个元素，强制求值头元素。
;;
;; 语法
;; ----
;; (stream-car strm)
;;
;; 参数
;; ----
;; strm : stream?
;; 非空流。
;;
;; 返回值
;; ----
;; any
;; 头元素的值。
;;
;; 说明
;; ----
;; 强制求值流对的头元素 promise 并返回记忆化的值。
;;
;; 错误处理
;; ----
;; type-error
;; 参数不是流
;; value-error
;; 参数是空流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-car "four"))
(check-catch 'value-error (stream-car stream-null))
(check (stream-car (stream 1 2 3)) => 1)
(check (stream-car (stream-from 0)) => 0)

(check-report)
