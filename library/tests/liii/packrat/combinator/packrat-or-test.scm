;; packrat-or
;; 构造择一解析的解析组合子。
;;
;; 语法
;; ----
;; (packrat-or p1 p2)
;;
;; 参数
;; ----
;; p1 : procedure
;; 首选解析组合子。
;; p2 : procedure
;; 备选解析组合子。
;;
;; 返回值
;; ----
;; procedure
;; 接收 <parse-results> 并返回 <parse-result> 的解析组合子。
;;
;; 说明
;; ----
;; 首选解析成功时直接返回其结果；
;; 失败时尝试备选解析，并把首选的错误合并进结果。

(import (liii check) (liii packrat combinator) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(let* ((gen (lambda () (values #f '(num . 777))))
       (%parse-num
         (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (%parse-id
         (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-id
       (%parse-or (packrat-or %parse-num %parse-id))
       (results (base-generator->results gen))
       (r (%parse-or results))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => 777)
) ;let*

;; 首选失败时回退到备选
(let* ((gen (lambda () (values #f '(id . foo))))
       (%parse-num
         (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (%parse-id
         (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-id
       (%parse-or (packrat-or %parse-num %parse-id))
       (results (base-generator->results gen))
       (r (%parse-or results))
      ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => 'foo)
) ;let*

(check-report)
