(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-for-each
;; 对字符串中的每个字符依次应用过程。
;;
;; 语法
;; ----
;; (string-for-each proc string1 string2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接受字符参数的过程。
;; string1, string2, ... : string?
;; 至少一个字符串。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. 按索引顺序依次调用 proc
;; 2. 多个字符串时，proc 接收对应位置的字符
;; 3. 遍历到最短字符串的长度为止
(let ((result '()))
  (string-for-each (lambda (c)
                     (set! result (cons c result))
                   ) ;lambda
    "abc"
  ) ;string-for-each
  (check result => '(#\c #\b #\a))
) ;let
(let ((result '()))
  (string-for-each (lambda (c1 c2)
                     (set! result (cons (list c1 c2) result))
                   ) ;lambda
    "ab"
    "xy"
  ) ;string-for-each
  (check result => '((#\b #\y) (#\a #\x)))
) ;let
(let ((result '()))
  (string-for-each (lambda (c)
                     (set! result (cons c result))
                   ) ;lambda
    ""
  ) ;string-for-each
  (check result => '())
) ;let
(check-catch 'wrong-number-of-args
  (string-for-each)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-for-each (lambda (c) c))
) ;check-catch

(check-report)
