;; list->stream
;; 把列表转换为元素相同的流。
;;
;; 语法
;; ----
;; (list->stream objs)
;;
;; 参数
;; ----
;; objs : list?
;; 待转换列表。
;; 返回值
;; ----
;; stream?
;; 元素顺序相同的流。
;;
;; 说明
;; ----
;; 错误：非列表参数报 non-list argument。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'list->stream (list->stream "four"))
(check (stream->list (list->stream '())) => '())
(check (stream->list (list->stream '(1 2 3))) => '(1 2 3))

(check-report)
