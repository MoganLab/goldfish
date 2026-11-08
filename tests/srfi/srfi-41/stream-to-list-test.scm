;; stream->list
;; 把流（前 n 个元素）转换为列表。
;;
;; 语法
;; ----
;; (stream->list [n] strm)
;;
;; 参数
;; ----
;; n : integer?
;; 可选元素数上限，非负。
;;
;; strm : stream?
;; 待转换流。
;; 返回值
;; ----
;; list?
;; 流元素列表；n 给出时至多 n 个元素。
;;
;; 说明
;; ----
;; SRFI 41：不带 n 时流必须有限。

;;
;; 错误处理
;; ----
;; type-error
;; strm 不是流或 n 不是整数
;; value-error
;; n 为负数

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream->list '()))
(check-catch 'type-error (stream->list "four" (stream 1)))
(check-catch 'value-error (stream->list -1 (stream 1)))
(check (stream->list (stream)) => '())
(check (stream->list (stream 1 2 3)) => '(1 2 3))
(check (stream->list 5 (stream 1 2 3)) => '(1 2 3))
(check (stream->list 3 (stream-from 1)) => '(1 2 3))

(check-report)
