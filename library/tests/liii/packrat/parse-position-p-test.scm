;; parse-position?
;; 判断对象是否为解析位置记录。
;;
;; 语法
;; ----
;; (parse-position? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <parse-position> 记录时返回 #t，否则返回 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(check-true (parse-position? (make-parse-position "test.scm" 3 15)))
(check-true (parse-position? (top-parse-position "test.scm")))

(check-false (parse-position? 42))
(check-false (parse-position? "test.scm"))
(check-false (parse-position? '()))
(check-false (parse-position? #f))

(check-report)
