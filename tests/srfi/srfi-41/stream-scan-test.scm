;; stream-scan
;; 折叠的部分序列流：base、(proc base e0)、...。
;;
;; 语法
;; ----
;; (stream-scan proc base strm)
;;
;; 参数
;; ----
;; proc : procedure?
;; 累积函数。
;;
;; base : any
;; 初始累积值。
;;
;; strm : stream?
;; 输入流。
;; 返回值
;; ----
;; stream?
;; 首元素为 base 的累积序列流，比输入长 1。
;;
;; 说明
;; ----
;; 错误：proc 非过程或 strm 非流分别报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-scan (stream-scan "four" 0 (stream 1)))
(check-catch 'stream-scan (stream-scan + 0 '()))
(check (stream->list (stream-scan + 0 (stream 1 2 3))) => '(0 1 3 6))
(check (stream->list 3 (stream-scan * 1 (stream-from 1))) => '(1 1 2))

(check-report)
