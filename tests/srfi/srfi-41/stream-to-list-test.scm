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
;; 错误：非流/非整数/负数参数分别报错。
;; SRFI 41：不带 n 时流必须有限。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream->list (stream->list '()))
(check-catch 'stream->list (stream->list "four" (stream 1)))
(check-catch 'stream->list (stream->list -1 (stream 1)))
(check (stream->list (stream)) => '())
(check (stream->list (stream 1 2 3)) => '(1 2 3))
(check (stream->list 5 (stream 1 2 3)) => '(1 2 3))
(check (stream->list 3 (stream-from 1)) => '(1 2 3))

(check-report)
