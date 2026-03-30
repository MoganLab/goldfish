(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; rational?
;; 判断一个对象是否是有理数（包括整数、有理数）。
;;
;; 语法
;; ----
;; (rational? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数、有理数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。

(check-true (rational? 123))        ; 整数
(check-true (rational? 1/2))        ; 有理数
(check-false (rational? 123.456))   ; 浮点数
(check-false (rational? 1+2i))      ; 复数
(check-false (rational? "123"))     ; 字符串
(check-false (rational? #t))        ; 布尔值
(check-false (rational? 'symbol))   ; 符号
(check-false (rational? '(1 2 3)))  ; 列表

(check-report)
