;; stream-take
;; 取流的前 n 个元素构成新流。
;;
;; 语法
;; ----
;; (stream-take n strm)
;;
;; 参数
;; ----
;; n : integer?
;; 非负整数。
;;
;; strm : stream?
;; 输入流。
;; 返回值
;; ----
;; stream?
;; 至多 n 个元素的流；流长不足 n 时取全部。
;;
;; 说明
;; ----
;; 错误：非整数/负数/非流参数分别报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-take (stream-take 5 "four"))
(check-catch 'stream-take (stream-take "four" (stream 1)))
(check-catch 'stream-take (stream-take -4 (stream 1)))
(check (stream->list (stream-take 5 stream-null)) => '())
(check (stream->list (stream-take 0 (stream 1 2 3))) => '())
(check (stream->list (stream-take 2 (stream 1 2 3))) => '(1 2))
(check (stream->list (stream-take 5 (stream 1 2 3))) => '(1 2 3))

(check-report)
