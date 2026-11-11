;; make-expected-result
;; 构造“期望某记号”类型的失败解析结果。
;;
;; 语法
;; ----
;; (make-expected-result pos str)
;;
;; 参数
;; ----
;; pos : <parse-position> 或 #f
;; 出错位置。
;; str : string
;; 期望的记号描述。
;;
;; 返回值
;; ----
;; <parse-result>
;; 失败的解析结果记录，其错误为 parse-error。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(let ((fail (make-expected-result (make-parse-position #f 1 0) "num")))
  (check-true (parse-result? fail))
  (check-false (parse-result-successful? fail))
  (check-true (parse-error? (parse-result-error fail)))
  (check (parse-error-expected (parse-result-error fail)) => '("num"))
) ;let

(check-report)
