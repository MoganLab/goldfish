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
;; 错误：非流参数或空流分别报 non-stream / null stream。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-cdr (stream-cdr "four"))
(check-catch 'stream-cdr (stream-cdr stream-null))
(check (stream-car (stream-cdr (stream 1 2 3))) => 2)
(check (stream->list (stream-cdr (stream 1 2 3))) => '(2 3))

(check-report)
