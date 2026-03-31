(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; symbol=?
;; 判断给定符号是否相等
;;
;; 语法
;; ----
;; (symbol=? symbol1 symbol2 ...)
;;
;; 参数
;; ----
;; symbol1, symbol2, ... : symbol?
;; 一个或多个符号参数
;;
;; 返回值
;; -----
;; boolean?
;; 如果所有符号相等则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 符号比较是基于符号的标识符名称进行的。
;; R7RS中规定symbol=?需要至少两个参数。

;; 基本测试
(check-catch 'wrong-number-of-args (symbol=? 'a))
(check-catch 'wrong-number-of-args (symbol=? 1))

(check-true (symbol=? 'a 'a))
(check-true (symbol=? 'foo 'foo))
(check-false (symbol=? 'a 'b))
(check-false (symbol=? 'foo 'bar))

;; 多参数测试
(check-true (symbol=? 'bar 'bar 'bar))
(check-true (symbol=? 'x 'x 'x 'x))
(check-false (symbol=? 'a 'a 'b))

;; 边界测试
(check-true (symbol=? 'a (string->symbol "a")))
(check-false (symbol=? 'a (string->symbol "A")))

;; 类型错误测试
(check-false (symbol=? 1 1))
(check-false (symbol=? 'a 1))
(check-false (symbol=? (string->symbol "foo") 1))
(check-false (symbol=? 'a 'b '()))

(check-report)
