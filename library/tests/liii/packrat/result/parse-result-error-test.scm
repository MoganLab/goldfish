;; parse-result-error
;; 读取解析结果携带的错误。
;;
;; 语法
;; ----
;; (parse-result-error result)
;;
;; 参数
;; ----
;; result : <parse-result>
;; 解析结果记录。
;;
;; 返回值
;; ----
;; <parse-error> 或 #f
;; 错误记录；成功结果为 #f。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

;; 成功结果没有错误
(check-false (parse-result-error (make-result 42 #f)))

;; 失败结果携带 parse-error
(let ((fail (make-expected-result (make-parse-position "f" 1 0) "num")))
  (check-true (parse-error? (parse-result-error fail)))
) ;let

(check-report)
