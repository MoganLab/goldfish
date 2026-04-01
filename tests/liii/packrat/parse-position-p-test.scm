(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

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
;;   待判断的对象
;;
;; 返回值
;; ----
;; bool
;;   #t 若为 parse-position 对象，#f 否则
;;
;; 描述
;; ----
;; 类型判断谓词，用于区分 parse-position 对象和其他类型。

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
) ;let

(let ()
  (define pos2 (make-parse-position #f 1 0))
  (check-true (parse-position? pos2))
) ;let

(let ()
  (check-false (parse-position? 42))
  (check-false (parse-position? "string"))
  (check-false (parse-position? 'symbol))
  (check-false (parse-position? (list 1 2 3)))
  (check-false (parse-position? #t))
  (check-false (parse-position? #f))
) ;let

(let ()
  (define result (make-result 42 #f))
  (check-false (parse-position? result))
) ;let

(check-report)
