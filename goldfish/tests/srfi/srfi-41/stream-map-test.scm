;; stream-map
;; 把过程应用到多个流的对应元素生成新流。
;;
;; 语法
;; ----
;; (stream-map proc strm ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受每个流的一个元素。
;; strm : stream?
;; 一个或多个流。
;;
;; 返回值
;; ----
;; stream?
;; 映射结果流；任一输入流到尽头即终止。
;;
;; 说明
;; ----
;;
;; 错误处理
;; ----
;; type-error
;; proc 不是过程或任一 strm 不是流
;; value-error
;; 没有提供流参数

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-map "four" (stream 1)))
(check-catch 'value-error (stream-map odd?))
(check-catch 'type-error (stream-map odd? "four"))
(check (stream->list (stream-map - (stream 1 2 3))) => '(-1 -2 -3))
(check (stream->list (stream-map + (stream 1 2 3) (stream 1 2 3))) => '(2 4 6))
(check (stream->list 3 (stream-map * (stream-from 1) (stream-from 1)))
  =>
  '(1 4 9)
) ;check

(check-report)
