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
;; 返回值
;; ----
;; any
;; 头元素的值。
;;
;; 说明
;; ----
;; 错误：非流参数或空流分别报 non-stream / null stream。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-car (stream-car "four"))
(check-catch 'stream-car (stream-car stream-null))
(check (stream-car (stream 1 2 3)) => 1)
(check (stream-car (stream-from 0)) => 0)

(check-report)
