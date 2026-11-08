;; stream-concat
;; 把流的流拼接展平为单个流。
;;
;; 语法
;; ----
;; (stream-concat strms)
;;
;; 参数
;; ----
;; strms : stream?
;; 元素全为流的流。
;; 返回值
;; ----
;; stream?
;; 所有内层流元素依次连接的流。
;;
;; 说明
;; ----
;; 错误：参数非流或内层元素非流分别报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-concat (stream-concat "four"))
(check (stream->list (stream-concat (stream (stream 1 2 3)))) => '(1 2 3))
(check (stream->list (stream-concat (stream (stream 1 2 3) (stream 1 2 3))))
  => '(1 2 3 1 2 3))

(check-report)
