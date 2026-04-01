(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; packrat-or
;; 构造尝试多个可选解析方案的 combinator。
;;
;; 语法
;; ----
;; (packrat-or comb1 comb2)
;;
;; 参数
;; ----
;; comb1 : procedure
;;   第一个 combinator
;; comb2 : procedure
;;   第二个 combinator
;;
;; 返回值
;; ----
;; procedure
;;   选择 combinator 函数
;;
;; 描述
;; ----
;; 按顺序尝试多个 combinator，返回第一个成功的结果。
;; 如果第一个失败，则尝试第二个。

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

(let* ((gen (generator '((num . 777))))
       (%parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-or (packrat-or %parse-num %parse-id)))
  (let ((r (%parse-or (base-generator->results gen))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 777)
  ) ;let
) ;let*

(let* ((gen (generator '((id . foo))))
       (%parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-or (packrat-or %parse-num %parse-id)))
  (let ((r (%parse-or (base-generator->results gen))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 'foo)
  ) ;let
) ;let*

(let* ((gen (generator '((string . "test"))))
       (%parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-or (packrat-or %parse-num %parse-id)))
  (let ((r (%parse-or (base-generator->results gen))))
    (check-false (parse-result-successful? r))
  ) ;let
) ;let*

(check-report)
