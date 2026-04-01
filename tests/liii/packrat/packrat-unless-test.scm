(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; packrat-unless
;; 构造排除规则 not-followed-by 的 combinator。
;;
;; 语法
;; ----
;; (packrat-unless message not-wanted fallback)
;;
;; 参数
;; ----
;; message : string
;;   失败时显示的消息
;; not-wanted : procedure
;;   不希望的 token 匹配 combinator
;; fallback : procedure
;;   备选 combinator
;;
;; 返回值
;; ----
;; procedure
;;   条件 combinator 函数
;;
;; 描述
;; ----
;; 如果 not-wanted 成功匹配，则失败；
;; 如果 not-wanted 失败，则使用 fallback。
;; 常用于排除特定模式（如"不是关键字"）。

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

(let ((gen-id (generator '((id . test)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-unless (packrat-unless "not expected" %parse-num %parse-id))
  (let ((r (%parse-unless (base-generator->results gen-id))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 'test)
  ) ;let
) ;let

(let ((gen-num (generator '((num . 42)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-unless (packrat-unless "not expected" %parse-num %parse-id))
  (let ((r (%parse-unless (base-generator->results gen-num))))
    (check-false (parse-result-successful? r))
  ) ;let
) ;let

(check-report)
