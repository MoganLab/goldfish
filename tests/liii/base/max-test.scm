(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; max
;; 返回所有给定实数的最大值。
;;
;; 语法
;; ----
;; (max num ...)
;;
;; 参数
;; ----
;; num : real?
;; 任意个实数（大于等于1）。
;;
;; 返回值
;; ------
;; real?
;; 返回所给所有值的最大值。
;; 如果存在NaN，返回NaN。
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 错误
;; ----
;; type-error
;; 如果存在任何参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果没有提供参数，抛出错误。

(check (max 7) => 7)  
(check (max 3.5) => 3.5) 
(check (max 1/3) => 1/3) 
(check (max +inf.0) => +inf.0) 
(check (max -inf.0) => -inf.0) 
(check (nan? (max +nan.0)) => #t) 


(check (max 7 3) => 7)  
(check (max 3.0 7.0) => 7.0)  
(check (max 3 7.0) => 7.0)  
(check (max 7.0 3) => 7.0)  
(check (max 1/2 1/3) => 1/2)  
(check (max 1/3 2/3) => 2/3)  
(check (max +inf.0 7) => +inf.0)  
(check (max 7 +inf.0) => +inf.0)  
(check (max -inf.0 7) => 7.0)  
(check (max 7 -inf.0) => 7.0)  
(check (nan? (max +nan.0 7)) => #t)  
(check (nan? (max 7 +nan.0)) => #t)  

(check (max 7 3 5) => 7)  
(check (max 3.0 7.0 2.0) => 7.0)  
(check (max 7 3.0 5) => 7.0)  
(check (max 1/2 1/3 2/3) => 2/3) 
(check (max +inf.0 7 3) => +inf.0)  
(check (max -inf.0 7 3) => 7.0) 
(check (nan? (max +nan.0 7 3)) => #t)  
(check (nan? (max 7 +nan.0 3)) => #t) 
(check (nan? (max +nan.0 +inf.0 -inf.0)) => #t) 

(check (max 7 3.0 5/4) => 7.0)  
(check (max 5.0 7/2 8) => 8.0)
(check (max +inf.0 7 3/4) => +inf.0)  
(check (max -inf.0 7 3.0) => 7.0) 
(check (nan? (max +nan.0 7.0 3)) => #t)  
(check (nan? (max 7/3 +nan.0 3)) => #t) 

(check-catch 'wrong-number-of-args (max))  
(check-catch 'type-error (max 'hello 7))  
(check-catch 'type-error (max "world" 7))  
(check-catch 'type-error (max #t 7))  
(check-catch 'type-error (max #f 7)) 
(check-catch 'type-error (max '(1 3 5) 7)) 
(check-catch 'type-error (max '() 7))  
(check-catch 'type-error (max 1+2i 2))  

(check-report)
