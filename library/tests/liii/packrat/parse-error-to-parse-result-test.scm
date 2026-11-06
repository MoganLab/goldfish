;; parse-error->parse-result
;; 把解析错误转换为失败的解析结果。
;;
;; 语法
;; ----
;; (parse-error->parse-result err)
;;
;; 参数
;; ----
;; err : <parse-error>
;; 解析错误记录。
;;
;; 返回值
;; ----
;; <parse-result>
;; 失败的解析结果记录，错误字段为该 parse-error。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let* ((pos (make-parse-position "test.scm" 2 10))
       (err (make-error-expected pos "open-paren"))
       (result (parse-error->parse-result err))
      ) ;
  (check-true (parse-result? result))
  (check-false (parse-result-successful? result))
  (check (parse-result-error result) => err)
) ;let*

(check-report)
