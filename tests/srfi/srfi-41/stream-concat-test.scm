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

;;
;; 错误处理
;; ----
;; type-error
;; 参数不是流，或流的元素不是流

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-concat "four"))
(check
  (stream->list (stream-concat (stream (stream 1 2 3))))
  =>
  '(1 2 3)
) ;check
(check
  (stream->list (stream-concat (stream (stream 1 2 3) (stream 1 2 3))))
  =>
  '(1 2 3 1 2 3)
) ;check

(check-report)
