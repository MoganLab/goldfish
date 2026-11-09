;; stream-drop-while
;; 丢弃流头部满足谓词的连续元素。
;;
;; 语法
;; ----
;; (stream-drop-while pred? strm)
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
;; 第一个不满足 pred? 的元素起的流。
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

(check-catch 'type-error (stream-drop-while "four" (stream 1)))
(check-catch 'type-error (stream-drop-while odd? "four"))
(check (stream->list (stream-drop-while odd? stream-null)) => '())
(check (stream->list (stream-drop-while odd? (stream 1 2 3))) => '(2 3))
(check (stream->list (stream-drop-while even? (stream 1 2 3))) => '(1 2 3))
(check (stream->list (stream-drop-while positive? (stream 1 2 3))) => '())

(check-report)
