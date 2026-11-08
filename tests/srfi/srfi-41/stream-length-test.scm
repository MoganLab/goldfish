;; stream-length
;; 返回有限流的元素个数。
;;
;; 语法
;; ----
;; (stream-length strm)
;;
;; 参数
;; ----
;; strm : stream?
;; 有限流。
;; 返回值
;; ----
;; integer?
;; 元素个数。
;;
;; 说明
;; ----
;; 无限流会不终止。

;;
;; 错误处理
;; ----
;; type-error
;; strm 不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-length "four"))
(check (stream-length (stream)) => 0)
(check (stream-length (stream 1 2 3)) => 3)
(check (stream-length (list->stream '(a b c d))) => 4)

(check-report)
