(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

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
;;   待判断的对象
;;
;; 返回值
;; ----
;; bool
;;   若为 parse-error 对象则返回 #t，否则返回 #f
;;
;; 描述
;; ----
;; 类型判断谓词，用于区分 parse-error 对象和其他类型。

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (check-true (parse-error? (make-error-expected pos "test")))
) ;let

(let ()
  (define pos (make-parse-position "main.gf" 5 3))
  (check-true (parse-error? (make-error-message pos "error")))
) ;let

(let ()
  (check-false (parse-error? 42))
  (check-false (parse-error? "string"))
  (check-false (parse-error? 'symbol))
  (check-false (parse-error? (list 1 2 3)))
  (check-false (parse-error? #t))
  (check-false (parse-error? #f))
) ;let

(let ()
  (define result (make-result 42 #f))
  (check-false (parse-error? result))
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 1 0))
  (check-false (parse-error? pos))
) ;let

(check-report)
