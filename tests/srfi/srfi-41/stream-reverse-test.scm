;; stream-reverse
;; 反转有限流。
;;
;; 语法
;; ----
;; (stream-reverse strm)
;;
;; 参数
;; ----
;; strm : stream?
;; 有限流。
;;
;; 返回值
;; ----
;; stream?
;; 元素顺序相反的流。
;;
;; 说明
;; ----
;;
;; 错误处理
;; ----
;; type-error
;; strm 不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-reverse '()))
(check (stream->list (stream-reverse (stream))) => '())
(check (stream->list (stream-reverse (stream 1 2 3))) => '(3 2 1))

(check-report)
