(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-results?
;; 判断对象是否为 parse-results 记录。
;;
;; 语法
;; ----
;; (parse-results? obj)
;;
;; 参数
;; ----
;; obj : any
;;   待判断的对象
;;
;; 返回值
;; ----
;; bool
;;   若为 parse-results 对象则返回 #t，否则返回 #f
;;
;; 描述
;; ----
;; 类型判断谓词，用于区分 parse-results 对象和其他类型。

(let ()
  (define gen (lambda () (values (make-parse-position "test" 1 0) #f)))
  (define results (base-generator->results gen))
  (check-true (parse-results? results))
) ;let

(let ()
  (check-false (parse-results? 42))
  (check-false (parse-results? "string"))
  (check-false (parse-results? 'symbol))
  (check-false (parse-results? (list 1 2 3)))
  (check-false (parse-results? #t))
  (check-false (parse-results? #f))
) ;let

(let ()
  (define result (make-result 42 #f))
  (check-false (parse-results? result))
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 1 0))
  (check-false (parse-results? pos))
) ;let

(check-report)
