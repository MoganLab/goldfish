;; parse-result-next
;; 读取解析结果的后续解析状态。
;;
;; 语法
;; ----
;; (parse-result-next result)
;;
;; 参数
;; ----
;; result : <parse-result>
;; 解析结果记录。
;;
;; 返回值
;; ----
;; <parse-results> 或 #f
;; 后续解析状态；输入结束时为 #f。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position) (liii hash-table))

(check-set-mode! 'report-failed)

;; 成功结果没有后续状态
(check-false (parse-result-next (make-result 42 #f)))

;; 后续状态为 parse-results 时原样返回
(let* ((gen (lambda () (values (make-parse-position "test" 1 0) #f)))
       (results (base-generator->results gen))
      ) ;
  (check-true (parse-results? (parse-result-next (make-result 42 results))))
) ;let*

(check-report)
