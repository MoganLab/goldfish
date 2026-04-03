(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; modulo
;; 计算实数的取模运算。
;;
;; 语法
;; ----
;; (modulo dividend divisor)
;;
;; 参数
;; ----
;; dividend : real? - 被除数
;; divisor : real? - 除数，不能为零
;;
;; 返回值
;; ------
;; real?
;; 返回 dividend 除以 divisor 的余数。
;;
;; 错误
;; ----
;; type-error
;; 当任一参数不是实数类型时抛出错误。包括复数（如 1+2i）、字符串、符号等其他类型。
;;
;; division-by-zero
;; 当除数 divisor 为零时抛出错误。
;;
;; wrong-number-of-args
;; 当参数数量不为两个时抛出错误。

(check (modulo 13 4) => 1)
(check (modulo -13 4) => 3)    
(check (modulo 13 -4) => -3)   
(check (modulo -13 -4) => -1)  
(check (modulo 0 5) => 0)    
(check (modulo 0 -5) => 0)    

(check (modulo 13 4.0) => 1.0)     
(check (modulo -13.0 4) => 3.0)    
(check (modulo 13.0 -4.0) => -3.0) 
(check (modulo 1000000 7) => 1)    
(check (modulo 1 1) => 0)
(check (modulo 5 5) => 0)
(check (modulo -1 5) => 4)
(check (modulo -5 5) => 0)
(check (modulo 20 7) => 6)
(check (modulo -20 7) => 1)
(check (modulo 20 -7) => -1)


(check-catch 'type-error (modulo 1+i 2))
(check-catch 'type-error (modulo 'hello 2))
(check-catch 'wrong-number-of-args (modulo 5))
(check-catch 'wrong-number-of-args (modulo 5 3 2))
(check-catch 'division-by-zero (modulo 1 0))

(check (floor-remainder 13 4) => 1)
(check (floor-remainder -13 4) => 3)    
(check (floor-remainder 13 -4) => -3)   
(check (floor-remainder -13 -4) => -1)  
(check (floor-remainder 0 5) => 0)    
(check (floor-remainder 0 -5) => 0)    

(check (floor-remainder 13 4.0) => 1.0)     
(check (floor-remainder -13.0 4) => 3.0)    
(check (floor-remainder 13.0 -4.0) => -3.0) 
(check (floor-remainder 1000000 7) => 1)    
(check (floor-remainder 1 1) => 0)
(check (floor-remainder 5 5) => 0)
(check (floor-remainder -1 5) => 4)
(check (floor-remainder -5 5) => 0)
(check (floor-remainder 20 7) => 6)
(check (floor-remainder -20 7) => 1)
(check (floor-remainder 20 -7) => -1)

(check-catch 'type-error (floor-remainder 1+i 2))
(check-catch 'type-error (floor-remainder 'hello 2))
(check-catch 'wrong-number-of-args (floor-remainder 5))
(check-catch 'wrong-number-of-args (floor-remainder 5 3 2))
(check-catch 'division-by-zero (floor-remainder 1 0))

(check-report)
