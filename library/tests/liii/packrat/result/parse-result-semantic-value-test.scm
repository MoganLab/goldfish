;; parse-result-semantic-value
;; 读取解析结果的语义值。
;;
;; 语法
;; ----
;; (parse-result-semantic-value result)
;;
;; 参数
;; ----
;; result : <parse-result>
;; 解析结果记录。
;;
;; 返回值
;; ----
;; any
;; 解析得到的语义值；失败结果为 #f。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(check (parse-result-semantic-value (make-result 42 #f)) => 42)
(check (parse-result-semantic-value (make-result 'foo #f)) => 'foo)

;; 失败结果没有语义值
(check-false (parse-result-semantic-value (make-expected-result #f "num")))

(check-report)
