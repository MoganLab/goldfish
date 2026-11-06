;; parse-results?
;; 判断对象是否为解析结果流记录。
;;
;; 语法
;; ----
;; (parse-results? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; obj 是 <parse-results> 记录时返回 #t，否则返回 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let* ((gen (lambda () (values (make-parse-position "test" 1 0) #f)))
       (results (base-generator->results gen))
      ) ;
  (check-true (parse-results? results))
) ;let*

(check-false (parse-results? 42))
(check-false (parse-results? '()))
(check-false (parse-results? #f))

(check-report)
