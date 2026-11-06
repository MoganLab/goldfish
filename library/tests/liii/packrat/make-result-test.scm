;; make-result
;; 构造成功的解析结果记录。
;;
;; 语法
;; ----
;; (make-result semantic-value next)
;;
;; 参数
;; ----
;; semantic-value : any
;; 解析得到的语义值。
;; next : <parse-results> 或 #f
;; 后续解析状态；为 #f 表示输入结束。
;;
;; 返回值
;; ----
;; <parse-result>
;; 成功的解析结果记录。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ((success (make-result 42 #f)))
  (check-true (parse-result? success))
  (check-true (parse-result-successful? success))
  (check (parse-result-semantic-value success) => 42)
  (check-false (parse-result-next success))
  (check-false (parse-result-error success))
) ;let

(check-report)
