(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; signature
;; 获取函数的类型签名（参数类型和返回值类型的约束）。
;;
;; 语法
;; ----
;; (signature func)
;;
;; 参数
;; ----
;; func : procedure?
;; 要获取类型签名的函数。
;;
;; 返回值
;; ------
;; pair? 或 #f
;; 返回一个点对，表示函数的签名 (return-type . arg-types)；
;; 如果函数没有签名信息，返回 #f。
;;
;; 说明
;; ----
;; 签名可能包含循环结构（用 #1=... 表示），表示接受可变数量的参数。
;; 例如 #1=(number? . #1#) 表示返回 number? 并接受任意数量的 number? 参数。

;; 测试 car/cdr 的完整签名（返回任意类型，需要 pair? 参数）
(check (signature car) => '(#t pair?))
(check (signature cdr) => '(#t pair?))

;; 测试 cons 函数签名（返回 pair?，接受两个任意类型参数）
(check (signature cons) => '(pair? #t #t))

;; 测试基本算术函数（返回 number?，接受可变 number? 参数）
(check (car (signature +)) => 'number?)
(check (car (signature -)) => 'number?)
(check (car (signature *)) => 'number?)
(check (car (signature /)) => 'number?)

;; 测试比较函数（返回 boolean?，接受可变 real? 参数）
(check (car (signature =)) => 'boolean?)
(check (car (signature <)) => 'boolean?)
(check (car (signature >)) => 'boolean?)

;; 测试 string-append 函数（返回 string?，接受可变 string? 参数）
(check (car (signature string-append)) => 'string?)

;; 测试自定义函数（没有签名）
(check (let ((f (lambda (x) (+ x 1)))) (signature f)) => #f)

(check-report)
