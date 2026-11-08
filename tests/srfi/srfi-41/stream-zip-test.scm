;; stream-zip
;; 把多个流按位置合并为元素列表的流。
;;
;; 语法
;; ----
;; (stream-zip strm ...)
;;
;; 参数
;; ----
;; strm : stream?
;; 一个或多个流。
;; 返回值
;; ----
;; stream?
;; 每个元素是各输入流同期元素组成的列表；任一流到尽头即终止。
;;
;; 说明
;; ----
;; 错误：无流参数或任一非流参数报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-zip (stream-zip))
(check-catch 'stream-zip (stream-zip "four"))
(check-catch 'stream-zip (stream-zip (stream 1) "four"))
(check (stream->list (stream-zip (stream 1 2 3) stream-null)) => '())
(check (stream->list (stream-zip (stream 1 2 3))) => '((1) (2) (3)))
(check (stream->list (stream-zip (stream 1 2 3) (stream 1 2 3)))
  => '((1 1) (2 2) (3 3)))
(check (stream->list (stream-zip (stream 1 2 3) (stream-from 1)))
  => '((1 1) (2 2) (3 3)))

(check-report)
