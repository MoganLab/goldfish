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

;;
;; 错误处理
;; ----
;; type-error
;; n 不是整数或 strm 不是流
;; value-error
;; n 为负数

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-drop "four" (stream 1)))
(check-catch 'value-error (stream-drop -1 (stream 1)))
(check-catch 'type-error (stream-drop 2 "four"))
(check (stream->list (stream-drop 0 (stream 1 2 3))) => '(1 2 3))
(check (stream->list (stream-drop 1 (stream 1 2 3))) => '(2 3))
(check (stream->list (stream-drop 5 (stream 1 2 3))) => '())

(check-report)
