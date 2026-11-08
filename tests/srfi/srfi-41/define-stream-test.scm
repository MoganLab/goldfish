;; define-stream
;; 定义返回流的过程，stream-lambda 的语法糖。
;;
;; 语法
;; ----
;; (define-stream (name formals) body ...)
;;
;; 参数
;; ----
;; name : 标识符
;; 过程名。
;;
;; formals : 形参列表
;;
;; body : expression ...
;; 返回值
;; ----
;; 定义一个过程，调用返回流。
;;
;; 说明
;; ----
;; SRFI 41：等价于 (define name (stream-lambda formals body ...))。

;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check
  (stream->list
    (let ()
      (define-stream (double strm)
        (if (stream-null? strm)
          stream-null
          (stream-cons (* 2 (stream-car strm)) (double (stream-cdr strm)))
        ) ;if
      ) ;define-stream
      (double (stream 1 2 3))
    ) ;let
  ) ;stream->list
  =>
  '(2 4 6)
) ;check
(define-stream (fibs a b) (stream-cons a (fibs b (+ a b))))
(check (stream->list 8 (fibs 0 1)) => '(0 1 1 2 3 5 8 13))

(check-report)
