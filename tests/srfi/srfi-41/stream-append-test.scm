;; stream-append
;; 顺序拼接多个流。
;;
;; 语法
;; ----
;; (stream-append strm ...)
;;
;; 参数
;; ----
;; strm : stream?
;; 零个或多个待拼接流。
;;
;; 返回值
;; ----
;; stream?
;; 依次包含各流元素的流。
;;
;; 说明
;; ----
;; 拼接在到达最后一个流之前是增量的。
;;
;; 错误处理
;; ----
;; type-error
;; 任一参数不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-append "four"))
(check (stream->list (stream-append (stream 1 2 3))) => '(1 2 3))
(check (stream->list (stream-append (stream 1 2 3) (stream 1 2 3)))
  =>
  '(1 2 3 1 2 3)
) ;check
(check (stream->list (stream-append (stream 1 2 3) stream-null)) => '(1 2 3))
(check (stream->list (stream-append stream-null (stream 1 2 3))) => '(1 2 3))

(check-report)
