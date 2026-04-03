;; (liii cut) 模块测试文件
;;
;; cut 和 cute 是 SRFI-26 提供的宏，用于创建部分应用（Partial Application）函数。
;; 它们允许你预先填充函数的部分参数，留下一些参数位置待后续传入。
;;
;; 核心用途：在不使用 lambda 的情况下快速创建函数闭包，适用于函数式编程中的参数预设。

;; ==== 语法说明 ====
;;
;; (cut <函数> <参数>...)
;;   - 创建一个新函数，<函数> 会被延迟到调用时执行
;;   - <> 表示单个参数占位符（slot），调用时按顺序填充
;;   - <...> 表示可变参数占位符（rest-slot），收集剩余所有参数
;;
;; (cute <函数> <参数>...)
;;   - 与 cut 类似，但参数在创建时立即求值（非调用时）
;;   - 适用于参数包含副作用（如 set!）的场景

;; ==== 常见用法示例 ====
;;
;; 示例1：预设函数的第一个参数
;;   ((cut + 10 <>) 5)  => 15
;;
;; 示例2：预设函数的中间参数
;;   ((cut list 'a <> 'c) 'b)  => (a b c)
;;
;; 示例3：使用多个占位符
;;   ((cut list <> <>) 'x 'y)  => (x y)
;;
;; 示例4：结合可变参数
;;   ((cut + 1 <...>) 2 3 4)  => 10
;;   ((cut <> 1 <...>) + 2 3)  => 6
;;
;; 示例5：将占位符作为函数位置
;;   ((cut <> 'arg) procedure?)  => #f  (procedure? 对符号返回 #f)

;; ==== cut vs cute 的区别 ====
;;
;; cut: 参数在调用时才求值
;;   (let ((a 1))
;;     (define f (cut + a <>))
;;     (set! a 10)
;;     (f 5))  => 15  (使用 a 的当前值 10)
;;
;; cute: 参数在创建时就求值
;;   (let ((a 1))
;;     (define f (cute + a <>))
;;     (set! a 10)
;;     (f 5))  => 6   (使用 a 创建时的值 1)

;; ==== 使用场景 ====
;;
;; 1. 快速创建单参数函数传递给 map/filter
;;    (map (cut * 2 <>) '(1 2 3))  => (2 4 6)
;;
;; 2. 预设回调函数的上下文参数
;;    (button :on-click (cut handle-click 'save))
;;
;; 3. 构建函数管道中的中间转换器
;;    (compose (cut string-append "prefix-" <>) number->string)

;; ==== 函数分类索引 ====

;; 一、部分应用宏
;;   cut              - 延迟求值的部分应用（参数在调用时求值）
;;   cute             - 立即求值的部分应用（参数在创建时求值）

;; ==== 单元测试 ====

(import (liii check)
        (liii cut)
) ;import

(check-set-mode! 'report-failed)

;; 基础占位符测试
(check ((cut list <> 'y <>) 'x 'z) => '(x y z))
(check ((cut + 1 <...>) 2 3) => 6)
(check ((cut + 1 <...>)) => 1)
(check ((cut <> 1 <...>) + 2 3) => 6)
(check ((cut list <> <> <...>) 1 2 3) => '(1 2 3))
(check ((cut list <> <> <...>) 1 2) => '(1 2))
(check ((cut + 1 2)) => 3)
(check ((cut <>) list) => ())
(check ((cut)) => ())
(check ((cut <> #t <...>) if 1 0) => 1)

;; 错误处理测试
(check-catch 'wrong-number-of-args ((cut list <> <>) 1))
(check-catch 'wrong-number-of-args ((cut list <> <> <...>) 1))
(check-catch 'syntax-error ((cut list <> <> <...> <>) 1 2 3))

;; cut vs cute 求值时机对比测试
(let* ((a 1)
       (f (cut <> (set! a 2))))
  (check a => 1)
  (check (f (lambda (x) x)) => 2)
  (check a => 2)
) ;let*

(let* ((a 1)
       (f (cute <> (set! a 2))))
  (check a => 2)
  (set! a 1)
  (check (f (lambda (x) x)) => 2)
  (check a => 1)
) ;let*

(check-report)
