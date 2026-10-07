(import (liii check) (liii cut))

(check-set-mode! 'report-failed)

;; cut
;; 参数特化宏（创建部分应用函数闭包，非占位符表达式在每次调用时求值）。
;;
;; 语法
;; ----
;; (cut <slot-or-expr> <slot-or-expr>*) → procedure
;; (cut <slot-or-expr> <slot-or-expr>* <...>) → procedure
;;
;; 参数
;; ----
;; <slot-or-expr> : <> | expression
;; <> 表示位置占位符（slot），调用时由实参按序填充；
;; <expression> 为普通表达式（non-slot），在最终特化函数被调用时求值；
;; <...> 为可变参数占位符（rest-slot），收集后续传入的所有剩余参数（必须位于末尾）。
;;
;; 返回值
;; -----
;; procedure
;; 返回一个新构建的过程（闭包）。
;;
;; 说明
;; ----
;; cut 宏来源于 SRFI-26。
;; 与 cute 的区别在于：cut 中的 non-slot 表达式是在每次调用返回的过程时求值，
;; 而 cute 是在过程创建（特化）时立即求值一次。
;;
;; 示例
;; ----
;; ((cut list 1 <> 3 <>) 2 4)          => (1 2 3 4)
;; ((cut list 1 <> 3 <...>) 2 4 5 6)    => (1 2 3 4 5 6)
;; ((cut + 1 <...>) 2 3 4)              => 10

;; SRFI-26 标准规范测试
(check ((cut list)) => '())
(check ((cut list <...>)) => '())
(check ((cut list 1)) => '(1))
(check ((cut list <>) 1) => '(1))
(check ((cut list <...>) 1) => '(1))
(check ((cut list 1 2)) => '(1 2))
(check ((cut list 1 <>) 2) => '(1 2))
(check ((cut list 1 <...>) 2) => '(1 2))
(check ((cut list 1 <...>) 2 3 4) => '(1 2 3 4))
(check ((cut list 1 <> 3 <>) 2 4) => '(1 2 3 4))
(check ((cut list 1 <> 3 <...>) 2 4 5 6) => '(1 2 3 4 5 6))

;; 延迟求值特性测试（每次调用时求值）
(check (let* ((x 'wrong) (y (cut list x)))
         (set! x 'ok)
         (y)) => '(ok))

(check (let ((a 0))
         (map (cut + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a) => 2)

;; 过程位置使用占位符
(check ((cut <>) list) => '())
(check ((cut <> 1 <...>) + 2 3) => 6)

;; 错误处理测试
(check-catch 'wrong-number-of-args ((cut list <> <>) 1))
(check-catch 'wrong-number-of-args ((cut list <> <> <...>) 1))

(check-report)

