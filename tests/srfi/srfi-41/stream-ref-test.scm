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
;; 错误：非流/非整数/负数/越界分别报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-ref (stream-ref '() 4))
(check-catch 'stream-ref (stream-ref (stream-from 0) 3.5))
(check-catch 'stream-ref (stream-ref (stream-from 0) -3))
(check-catch 'stream-ref (stream-ref (stream 1 2 3) 5))
(check (stream-ref (stream 1 2 3) 0) => 1)
(check (stream-ref (stream 1 2 3) 1) => 2)
(check (stream-ref (stream 1 2 3) 2) => 3)
(check (stream-ref (stream-from 0) 100) => 100)

(check-report)
