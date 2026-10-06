(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match-letrec
;; 递归匹配并绑定变量，类似于 letrec，在所有表达式中都能访问所有绑定的变量（常用于定义互相递归的过程）。
;;
;; 语法
;; ----
;; (match-letrec ((pattern expr) ...) body ...)
;;
;; 参数
;; ----
;; pattern : pattern
;; 用于解构对应 expr 值的模式。
;;
;; expr : any
;; 待求值并进行模式解构的表达式。
;;
;; body : any
;; 在绑定作用域内执行的主体表达式。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

;; 互相递归函数解构绑定
(check
  (match-letrec
   (((even? odd?)
     (list
       (lambda (n) (if (zero? n) #t (odd? (- n 1))))
       (lambda (n) (if (zero? n) #f (even? (- n 1))))
     ) ;list
    ) ;
   ) ;
   (even? 10)
  ) ;match-letrec
  =>
  #t
) ;check

(check
  (match-letrec
   (((even? odd?)
     (list
       (lambda (n) (if (zero? n) #t (odd? (- n 1))))
       (lambda (n) (if (zero? n) #f (even? (- n 1))))
     ) ;list
    ) ;
   ) ;
   (odd? 11)
  ) ;match-letrec
  =>
  #t
) ;check

;; 单个递归函数测试
(check
  (match-letrec
   ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))))
   (fact 5)
  ) ;match-letrec
  =>
  120
) ;check

;; 匹配失败抛出 match-error
(check-catch 'match-error (match-letrec (((x y) '(1))) (+ x y)))

(check-report)
