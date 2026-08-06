(import (liii check))
(import (scheme let))


(check-set-mode! 'report-failed)


;; symbol->dynamic-value
;; 返回符号的动态绑定值。
;;
;; 语法
;; ----
;; (symbol->dynamic-value sym)
;;
;; 参数
;; ----
;; sym : symbol?
;; 要查询的符号。
;;
;; 返回值
;; ------
;; any
;; 该符号的动态绑定值。
;;
;; 说明
;; ----
;; symbol->dynamic-value 返回符号的动态绑定。
;; 动态绑定与词法绑定 (symbol->value) 不同：
;; 动态绑定反映符号在调用栈中的动态环境。
;; 若符号无动态绑定，返回 #<undefined>。


;; 未绑定的符号返回 #<undefined>
(check (undefined? (symbol->dynamic-value 'nonexistent-xyz)) => #t)


;; symbol->dynamic-value 接受符号参数，返回某个值
(check (symbol? 'x) => #t)


;; 对内建符号，dynamic-value 与 value 一致
(check (eq? (symbol->dynamic-value '+) (symbol->value '+)) => #t)


;; 在当前环境中，局部变量的 dynamic-value
(check (let ((x 42)) (symbol->dynamic-value 'x)) => 42)


(check-report)
