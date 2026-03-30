(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; exact-integer?
;; 判断一个数值是否为精确整数。
;;
;; 语法
;; ----
;; (exact-integer? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; -----
;; boolean?
;; 如果 obj 是精确数值且为整数返回 #t，否则返回 #f。

(check-true (exact-integer? 42))        ; 精确整数
(check-true (exact-integer? -42))       ; 精确负数
(check-true (exact-integer? 0))         ; 零
;(check-true (exact-integer? #e42.0))   精确浮点数转换为整数(暂不支持此类数)
(check-false (exact-integer? 42.0))     ; 不精确整数
(check-false (exact-integer? 1/2))      ; 有理数
(check-false (exact-integer? 3.14))     ; 不精确浮点数
(check-false (exact-integer? 1+2i))     ; 复数
(check-false (exact-integer? "42"))     ; 字符串
(check-false (exact-integer? #t))       ; 布尔值
(check-false (exact-integer? 'symbol))  ; 符号

(check-report)
