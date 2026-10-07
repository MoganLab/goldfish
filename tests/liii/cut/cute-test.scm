(import (liii check) (liii cut))

(check-set-mode! 'report-failed)

;; cute
;; 参数特化宏（创建部分应用函数闭包，非占位符表达式在特化/创建时立即求值）。
;;
;; 语法
;; ----
;; (cute <slot-or-expr> <slot-or-expr>*) → procedure
;; (cute <slot-or-expr> <slot-or-expr>* <...>) → procedure
;;
;; 参数
;; ----
;; <slot-or-expr> : <> | expression
;; <> 表示位置占位符（slot），调用时由实参按序填充；
;; <expression> 为普通表达式（non-slot），在 cute 宏特化（即过程构建）时立即求值并绑定；
;; <...> 为可变参数占位符（rest-slot），收集后续传入的所有剩余参数（必须位于末尾）。
;;
;; 返回值
;; -----
;; procedure
;; 返回一个新构建的过程（闭包）。
;;
;; 说明
;; ----
;; cute 宏来源于 SRFI-26，名称意为 "cut with evaluation"。
;; 与 cut 的区别在于：cut 中的非占位符表达式延迟至调用时求值（每次调用都重新求值）；
;; 而 cute 中的非占位符表达式在宏特化（创建过程）时立即求值一次并局部绑定。
;;
;; 示例
;; ----
;; ((cute list 1 <> 3 <>) 2 4)         => (1 2 3 4)
;; ((cute list 1 <> 3 <...>) 2 4 5 6)   => (1 2 3 4 5 6)
;; ((cute + 1 <...>) 2 3 4)             => 10

;; SRFI-26 标准规范测试
(check ((cute list)) => '())
(check ((cute list <...>)) => '())
(check ((cute list 1)) => '(1))
(check ((cute list <>) 1) => '(1))
(check ((cute list <...>) 1) => '(1))
(check ((cute list 1 2)) => '(1 2))
(check ((cute list 1 <>) 2) => '(1 2))
(check ((cute list 1 <...>) 2) => '(1 2))
(check ((cute list 1 <...>) 2 3 4) => '(1 2 3 4))
(check ((cute list 1 <> 3 <>) 2 4) => '(1 2 3 4))
(check ((cute list 1 <> 3 <...>) 2 4 5 6) => '(1 2 3 4 5 6))

;; 立即求值特性测试（创建过程时求值一次）
(check (let ((a 0))
         (map (cute + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a) => 1)

;; 过程位置使用占位符
(check ((cute <>) list) => '())
(check ((cute <> 1 <...>) + 2 3) => 6)

;; 副作用与求值时机对比测试
(let* ((a 1) (f (cute <> (set! a 2))))
  (check a => 2)
  (set! a 1)
  (check (f (lambda (x) x)) => 2)
  (check a => 1)
) ;let*

;; 错误处理测试
(check-catch 'wrong-number-of-args ((cute list <> <>) 1))
(check-catch 'wrong-number-of-args ((cute list <> <> <...>) 1))

(check-report)
