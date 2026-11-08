;; stream-lambda
;; 构造返回流的惰性过程。
;;
;; 语法
;; ----
;; (stream-lambda formals body ...)
;;
;; 参数
;; ----
;; formals : 形参列表
;; 同 lambda。
;; body : expression ...
;; 过程体。
;;
;; 返回值
;; ----
;; procedure
;; 调用时返回流，过程体延迟执行。
;;
;; 说明
;; ----
;; SRFI 41：stream-lambda 体在每次调用产出新的惰性流，是定义无限流的基础。
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check
  (stream->list
    (letrec ((double
               (stream-lambda (strm)
                 (if (stream-null? strm)
                   stream-null
                   (stream-cons (* 2 (stream-car strm)) (double (stream-cdr strm)))
                 ) ;if
               ) ;stream-lambda
             ) ;double
            ) ;
      (double (stream 1 2 3))
    ) ;letrec
  ) ;stream->list
  =>
  '(2 4 6)
) ;check
(check
  (stream-ref ((stream-lambda () (stream-from 5))) 2)
  =>
  7
) ;check

(check-report)
