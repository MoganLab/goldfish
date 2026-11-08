;; stream
;; 用元素列表语法构造流。
;;
;; 语法
;; ----
;; (stream obj ...)
;;
;; 参数
;; ----
;; obj : any
;; 流元素，零个或多个。
;; 返回值
;; ----
;; stream?
;; 元素依次为 obj ... 的流；无参数时为空流。
;;
;; 说明
;; ----
;; SRFI 41：(stream) 等价于 stream-null。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream->list (stream)) => '())
(check (stream->list (stream 1)) => '(1))
(check (stream->list (stream 1 2 3)) => '(1 2 3))
(check (stream-null? (stream)) => #t)

(check-report)
