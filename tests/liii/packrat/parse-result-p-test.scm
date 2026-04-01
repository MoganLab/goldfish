(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-result?
;; 判断对象是否为 parse-result 对象。
;;
;; 语法
;; ----
;; (parse-result? obj)
;;
;; 参数
;; ----
;; obj : any
;;   待判断的对象
;;
;; 返回值
;; ----
;; bool
;;   若为 parse-result 对象则返回 #t，否则返回 #f
;;
;; 描述
;; ----
;; 类型判断谓词，用于区分 parse-result 对象和其他类型。

(let ()
  (check-true (parse-result? (make-result 42 #f)))
  (check-false (parse-result? 42))
  (check-false (parse-result? "hello"))
  (check-false (parse-result? 'symbol))
  (check-false (parse-result? (list 1 2 3)))
  (check-false (parse-result? #t))
) ;let

(let ()
  (define fail-result (make-expected-result (make-parse-position #f 1 0) "num"))
  (check-true (parse-result? fail-result))
) ;let

(check-report)
