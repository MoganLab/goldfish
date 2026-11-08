;; stream-cdr
;; 返回流的剩余部分（尾流）。
;;
;; 语法
;; ----
;; (stream-cdr strm)
;;
;; 参数
;; ----
;; strm : stream?
;; 非空流。
;; 返回值
;; ----
;; stream?
;; 尾流。
;;
;; 说明
;; ----
;; 返回流对的尾流 promise，不解引头元素。

;;
;; 错误处理
;; ----
;; type-error
;; 参数不是流
;; value-error
;; 参数是空流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-cdr "four"))
(check-catch 'value-error (stream-cdr stream-null))
(check (stream-car (stream-cdr (stream 1 2 3))) => 2)
(check (stream->list (stream-cdr (stream 1 2 3))) => '(2 3))

(check-report)
