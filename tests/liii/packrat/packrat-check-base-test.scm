(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; packrat-check-base
;; 构造匹配指定类型 token 的基础 combinator。
;;
;; 语法
;; ----
;; (packrat-check-base kind acceptor)
;;
;; 参数
;; ----
;; kind : kind-object
;;   token 类型标识符
;; acceptor : procedure
;;   语义值接受器函数，接收 token 语义值并返回 combinator
;;
;; 返回值
;; ----
;; procedure
;;   token 匹配 combinator 函数
;;
;; 描述
;; ----
;; 这是最基础的 combinator，用于匹配特定类型的 token。
;; 当 token 类型匹配时，调用 acceptor 函数处理语义值。

(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
          (values #f #f)
          (let ((token (car stream)))
            (set! stream (cdr stream))
            (values #f token)
          ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define

(let ((gen (generator '((num . 42)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define result (%parse-num (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 42)
) ;let

(let ((gen (generator '((id . foo)))))
  (define %parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
  (define result (%parse-id (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 'foo)
) ;let

(let ((gen (generator '((num . 123)))))
  (define %parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
  (define result (%parse-id (base-generator->results gen)))
  (check-false (parse-result-successful? result))
) ;let

(check-report)
