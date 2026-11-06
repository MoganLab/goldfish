;; packrat-check-base
;; 构造检查当前基础记号类型的解析组合子。
;;
;; 语法
;; ----
;; (packrat-check-base token-kind k)
;;
;; 参数
;; ----
;; token-kind : symbol 或 #f
;; 期望的记号类型；为 #f 时表示期望输入结束。
;; k : procedure
;; 接收记号值的过程，返回接受后续状态的解析组合子。
;;
;; 返回值
;; ----
;; procedure
;; 接收 <parse-results> 并返回 <parse-result> 的解析组合子。
;;
;; 说明
;; ----
;; 记号类型匹配时调用 k 处理记号值；
;; 不匹配时返回“期望该记号”的失败结果。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let* ((gen (lambda () (values #f '(num . 42))))
       (results (base-generator->results gen))
       (%parse-num
         (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (result (%parse-num results))
      ) ;
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 42)
) ;let*

;; 类型不匹配时返回期望结果
(let* ((gen (lambda () (values #f '(id . foo))))
       (results (base-generator->results gen))
       (%parse-num
         (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r))))
       ) ;%parse-num
       (result (%parse-num results))
      ) ;
  (check-false (parse-result-successful? result))
  (check (parse-error-expected (parse-result-error result)) => '(num))
) ;let*

(check-report)
