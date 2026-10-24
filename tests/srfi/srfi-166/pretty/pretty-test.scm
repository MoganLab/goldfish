(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

;; pretty
;; S 表达式代码格式化组合子。根据 Scheme 语言的缩进语法规则（如 define, lambda, let 等）进行美化换行与对齐排版。
;;
;; 语法
;; ----
;; (pretty obj)
;; (pretty-shared obj)
;; (pretty-simply obj)
;; (pretty-with-color obj)
;;
;; 示例
;; ----
;; (show #f (pretty '(foo bar))) => "(foo bar)\n"

(check (show #f (pretty '(foo bar))) => "(foo bar)\n")

(check (show #f (pretty '(define (fold kons knil ls)
                           (define (loop ls acc)
                             (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))
                           (loop ls knil))))
       => "(define (fold kons knil ls)\n  (define (loop ls acc)\n    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))\n  (loop ls knil))\n")

(check (show #f (pretty (let ((ones (list 1)))
                           (set-cdr! ones ones)
                           (list 'let (list (list 'ones (list 'quote ones))) 'ones))))
       => "(let ((ones '#0=(1 . #0#))) ones)\n")

(check-report)
