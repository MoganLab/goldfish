;; call-args-ok?
;; 判断签名是否与调用实参列表兼容（数量与类型）。
;;
;; 语法
;; ----
;; (call-args-ok? sig expr env locals)
;;
;; 参数
;; ----
;; sig : list        单条签名（如 '(string? -> integer?)）
;; expr : list       调用表达式（如 '(f "x")）
;; env : alist       类型环境
;; locals : alist    局部类型绑定
;;
;; 返回值
;; ------
;; boolean
;; 实参数量与类型均兼容时 #t。

(import (scheme base) (liii check) (liii goldcheck type-hint type))

(check-set-mode! 'report-failed)

(define env
  (list (cons 'my-len '((string? -> integer?)))
        (cons 'my-assoc '((any? alist? -> (or #f pair?))))
       ) ;list
) ;define

;; 1. 数量与类型均匹配
(check-true (call-args-ok? '(string? -> integer?) '(my-len "x") env '()))

;; 2. 数量不符
(check (call-args-ok? '(string? -> integer?) '(my-len "x" "y") env '()) => #f)

;; 3. 类型确定冲突：布尔值传 string? 参数
(check (call-args-ok? '(string? -> integer?) '(my-len #t) env '()) => #f)

;; 4. unknown（局部变量无类型）不判冲突
(check-true (call-args-ok? '(string? -> integer?) '(my-len x) env '()))

;; 5. 点对尾的调用形式跳过
(check (call-args-ok? '(string? -> integer?) '(my-len . rest) env '()) => #f)

(check-report)
