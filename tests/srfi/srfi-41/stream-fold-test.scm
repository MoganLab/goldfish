;; stream-fold
;; 从左向右折叠流。
;;
;; 语法
;; ----
;; (stream-fold proc base strm)
;;
;; 参数
;; ----
;; proc : procedure?
;; (proc base elem) 折叠函数。
;;
;; base : any
;; 初始累积值。
;;
;; strm : stream?
;; 有限流。
;; 返回值
;; ----
;; any
;; 折叠结果。
;;
;; 说明
;; ----
;; SRFI 41：故意不提供右折叠，需要全部物化时建议用列表。

;;
;; 错误处理
;; ----
;; type-error
;; proc 不是过程或 strm 不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-fold "four" 0 (stream 1)))
(check-catch 'type-error (stream-fold + 0 '()))
(check (stream-fold + 0 (stream 1 2 3)) => 6)
(check (stream-fold * 1 (stream 1 2 3 4)) => 24)

(check-report)
