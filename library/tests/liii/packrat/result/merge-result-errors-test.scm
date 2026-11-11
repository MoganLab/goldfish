;; merge-result-errors
;; 把额外的解析错误合并进解析结果。
;;
;; 语法
;; ----
;; (merge-result-errors result errs)
;;
;; 参数
;; ----
;; result : <parse-result>
;; 解析结果记录。
;; errs : <parse-error> 或 #f
;; 要合并的错误；为 #f 时不改变结果。
;;
;; 返回值
;; ----
;; <parse-result>
;; 合并错误后的新解析结果记录。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

;; 合并空错误：结果保持成功
(let ((result (merge-result-errors (make-result 42 #f) #f)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 42)
) ;let

;; 合并真实错误：错误字段被填充
(let* ((pos (make-parse-position "f" 1 0))
       (err (make-error-message pos "boom"))
       (result (merge-result-errors (make-result 42 #f) err))
      ) ;
  (check-true (parse-result-successful? result))
  (check (parse-result-error result) => err)
) ;let*

(check-report)
