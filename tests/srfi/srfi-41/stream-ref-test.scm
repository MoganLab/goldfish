;; stream-ref
;; 返回流第 n 个元素（从 0 计）。
;;
;; 语法
;; ----
;; (stream-ref strm n)
;;
;; 参数
;; ----
;; strm : stream?
;; 输入流。
;;
;; n : integer?
;; 非负索引。
;; 返回值
;; ----
;; any
;; 第 n 个元素。
;;
;; 说明
;; ----

;;
;; 错误处理
;; ----
;; type-error
;; strm 不是流或 n 不是整数
;; value-error
;; n 为负数，或超出流的长度

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-ref '() 4))
(check-catch 'type-error (stream-ref (stream-from 0) 3.5))
(check-catch 'value-error (stream-ref (stream-from 0) -3))
(check-catch 'value-error (stream-ref (stream 1 2 3) 5))
(check (stream-ref (stream 1 2 3) 0) => 1)
(check (stream-ref (stream 1 2 3) 1) => 2)
(check (stream-ref (stream 1 2 3) 2) => 3)
(check (stream-ref (stream-from 0) 100) => 100)

(check-report)
