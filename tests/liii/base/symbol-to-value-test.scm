(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; symbol->value
;; 返回符号在指定环境（默认 curlet）中的绑定值。
;;
;; 语法
;; ----
;; (symbol->value sym)
;; (symbol->value sym let)
;;
;; 参数
;; ----
;; sym : symbol?
;; 要查询的符号。
;;
;; let : let? 可选，默认为 (curlet)
;; 查询的环境。
;;
;; 返回值
;; ------
;; any
;; 该符号在给定环境中的绑定值。
;;
;; 说明
;; ----
;; symbol->value 返回符号的绑定值。
;; 单参数形式在当前环境 (curlet) 中查找。
;; 双参数形式在指定 let 中查找。
;; 若符号未绑定，返回 #<undefined>。


;; 内建符号 + 的值
(check (symbol->value '+) => +)


;; 内建符号 car 的值
(check (symbol->value 'car) => car)


;; 在指定 let 中查找符号
(check (symbol->value 'a (inlet 'a 99)) => 99)


;; 在当前环境中查找局部变量
(check (let ((x 42)) (symbol->value 'x)) => 42)


;; 未绑定的符号返回 #<undefined>
(check (undefined? (symbol->value 'nonexistent-xyz)) => #t)


;; 在指定 let 中查找字符串绑定
(check (symbol->value 'name (inlet 'name "hello")) => "hello")


(check-report)
