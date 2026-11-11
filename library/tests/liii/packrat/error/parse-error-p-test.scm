;; parse-error?
;; 判断对象是否为解析错误记录。
;;
;; 语法
;; ----
;; (parse-error? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <parse-error> 记录时返回 #t，否则返回 #f。

(import (liii check) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(check-true (parse-error? (make-error-expected (make-parse-position "f" 1 0) "num"))
) ;check-true
(check-true (parse-error? (make-error-message (make-parse-position "f" 1 0) "err"))
) ;check-true

(check-false (parse-error? 42))
(check-false (parse-error? '()))
(check-false (parse-error? #f))

(check-report)
