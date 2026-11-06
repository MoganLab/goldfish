;; parse-result-successful?
;; 读取解析结果是否成功。
;;
;; 语法
;; ----
;; (parse-result-successful? result)
;;
;; 参数
;; ----
;; result : <parse-result>
;; 解析结果记录。
;;
;; 返回值
;; ----
;; boolean
;; 解析成功时返回 #t，失败时返回 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(check-true (parse-result-successful? (make-result 42 #f)))
(check-false (parse-result-successful? (make-expected-result (make-parse-position #f 1 0) "num")
             ) ;parse-result-successful?
) ;check-false
(check-false (parse-result-successful? (make-message-result (make-parse-position #f 1 0) "err")
             ) ;parse-result-successful?
) ;check-false

(check-report)
