(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; packrat-check
;; 构造变换解析结果的 combinator。
;;
;; 语法
;; ----
;; (packrat-check comb transformer)
;;
;; 参数
;; ----
;; comb : procedure
;;   基础 combinator
;; transformer : procedure
;;   结果变换函数，接收解析结果语义值并返回 combinator
;;
;; 返回值
;; ----
;; procedure
;;   变换 combinator 函数
;;
;; 描述
;; ----
;; 在基础 combinator 成功后，对结果进行变换。
;; 常用于语义动作处理和结果转换。

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

(let ((gen (generator '((num . 25)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-check (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (* n 2) r)))))
  (define result (%parse-check (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 50)
) ;let

(let ((gen (generator '((num . 10)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-add5 (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (+ n 5) r)))))
  (define result (%parse-add5 (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 15)
) ;let

(let ((gen (generator '((id . foo)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-check (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (* n 2) r)))))
  (define result (%parse-check (base-generator->results gen)))
  (check-false (parse-result-successful? result))
) ;let

(check-report)
