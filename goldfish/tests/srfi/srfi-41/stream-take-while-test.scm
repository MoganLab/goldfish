;; stream-take-while
;; 取流头部满足谓词的连续元素。
;;
;; 语法
;; ----
;; (stream-take-while pred? strm)
;;
;; 参数
;; ----
;; pred? : procedure?
;; 谓词。
;; strm : stream?
;; 输入流。
;;
;; 返回值
;; ----
;; stream?
;; 头部连续满足 pred? 的元素流。
;;
;; 说明
;; ----
;;
;; 错误处理
;; ----
;; type-error
;; pred? 不是过程或 strm 不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-take-while odd? "four"))
(check-catch 'type-error (stream-take-while "four" (stream 1)))
(check (stream->list (stream-take-while odd? (stream 1 2 3))) => '(1))
(check (stream->list (stream-take-while even? (stream 1 2 3))) => '())
(check (stream->list (stream-take-while positive? (stream 1 2 3))) => '(1 2 3))
(check (stream->list (stream-take-while negative? (stream 1 2 3))) => '())

(check-report)
