;; stream-drop
;; 丢弃流的前 n 个元素。
;;
;; 语法
;; ----
;; (stream-drop n strm)
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
;; 跳过前 n 个元素后的流；流长不足 n 时为空流。
;;
;; 说明
;; ----
;; 错误：非整数/负数/非流参数分别报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-drop (stream-drop "four" (stream 1)))
(check-catch 'stream-drop (stream-drop -1 (stream 1)))
(check-catch 'stream-drop (stream-drop 2 "four"))
(check (stream->list (stream-drop 0 (stream 1 2 3))) => '(1 2 3))
(check (stream->list (stream-drop 1 (stream 1 2 3))) => '(2 3))
(check (stream->list (stream-drop 5 (stream 1 2 3))) => '())

(check-report)
