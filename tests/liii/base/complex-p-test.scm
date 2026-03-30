(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; complex?
;; 判断一个对象是否是复数（包括整数、浮点数、有理数、复数）。
;;
;; 语法
;; ----
;; (complex? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是数值类型（整数、浮点数、有理数、复数）返回 #t，否则返回 #f。
;;
;; 错误
;; ----
;; 无错误情况。

(check-true (complex? 1+2i))        ; 复数
(check-true (complex? 123))         ; 整数也是复数
(check-true (complex? 123.456))     ; 浮点数也是复数
(check-true (complex? 1/2))         ; 有理数也是复数
(check-false (complex? "123"))      ; 字符串
(check-false (complex? #t))         ; 布尔值
(check-false (complex? 'symbol))    ; 符号
(check-false (complex? '(1 2 3)))   ; 列表

(check-report)
