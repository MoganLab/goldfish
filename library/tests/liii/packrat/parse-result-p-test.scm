;; parse-result?
;; 判断对象是否为解析结果记录。
;;
;; 语法
;; ----
;; (parse-result? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <parse-result> 记录时返回 #t，否则返回 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(check-true (parse-result? (make-result 42 #f)))
(check-true (parse-result? (make-expected-result #f "num")))

(check-false (parse-result? 42))
(check-false (parse-result? '()))
(check-false (parse-result? #f))

(check-report)
