;; packrat-check
;; 构造对子解析结果做后置处理的解析组合子。
;;
;; 语法
;; ----
;; (packrat-check parser k)
;;
;; 参数
;; ----
;; parser : procedure
;; 子解析组合子。
;; k : procedure
;; 接收子解析语义值的过程，返回接受后续状态的解析组合子。
;;
;; 返回值
;; ----
;; procedure
;; 接收 <parse-results> 并返回 <parse-result> 的解析组合子。
;;
;; 说明
;; ----
;; 子解析成功时调用 k 处理语义值；
;; 子解析失败时原样返回失败结果。

(import (liii check) (liii packrat combinator) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(let* ((gen (lambda () (values #f '(num . 25))))
       (results (base-generator->results gen))
       (%parse-num
         (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (%parse-check
         (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (* n 2) r))))
       ) ;%parse-check
       (result (%parse-check results))
      ) ;
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 50)
) ;let*

(check-report)
