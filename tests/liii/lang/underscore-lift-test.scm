;; _ 占位符自动提升
;; 谓词位含 _ 时自动提升为单参 lambda（对标 Scala 的 _ 简写）。
;;
;; 语法
;; ----
;; ($ xs :map (op args ... _ ... args ...))
;; ($ xs :filter (op args ... _ ... args ...))
;;
;; 描述
;; ----
;; 含 _ 的谓词位提升为 (lambda (_) <表达式>)，多个 _ 绑定同一参数；
;; 不含 _ 的谓词位（如 odd?、现成 lambda）原样传递。
;; 支持形态：
;; - 扁平：_ 在操作数第 1~5 位，如 (+ _ 2)、(max 7 _ 1)；
;; - 深层：含 _ 的子表达式在第 1~3 位，如 (+ (* _ 2) 1)；
;; - 恒等：裸 _ 即 (lambda (_) _)。
;; 超出枚举位的形态（如更深层嵌套）会在运行时报 unbound variable _，
;; 请改用显式 lambda。

(import (liii check) (liii lang) (liii error))

(check-set-mode! 'report-failed)

;; === 扁平：_ 在各实参位 ===
(check ($ '(1 2 3) :map (+ _ 2)) => '(3 4 5))
(check ($ '(1 2 3) :map (- 10 _)) => '(9 8 7))
(check ($ '(1 2 3) :map (max 7 _ 1)) => '(7 7 7))
(check ($ '(1 2 3) :map (expt 2 _)) => '(2 4 8))
(check ($ '("a" "b") :map (string-append "x" _)) => '("xa" "xb"))

;; === 深层嵌套 ===
(check ($ '(1 2 3) :map (+ (* _ 2) 1)) => '(3 5 7))
(check ($ '(1 2 3) :map (* (+ _ 1) 2)) => '(4 6 8))
(check ($ '(1 2 3) :map (list (* _ 2) (+ _ 1))) => '((2 2) (4 3) (6 4)))

;; === 多个 _ 绑定同一参数 ===
(check ($ '(1 2 3) :map (list 0 _ _ 9)) => '((0 1 1 9) (0 2 2 9) (0 3 3 9)))

;; === 恒等 ===
(check ($ '(1 2 3) :map _) => '(1 2 3))
(check ($ #(1 2 3) :map _) => #(1 2 3))

;; === 无 _ 原样传递 ===
(check ($ '(1 2 3 4) :filter odd?) => '(1 3))
(check ($ '(1 -2 3) :map abs) => '(1 2 3))
(check ($ '(1 2 3) :map (lambda (x) (* x 10))) => '(10 20 30))

;; === if 表达式 / 谓词形态 ===
(check ($ '(1 2 3) :map (if (odd? _) 'o 'e)) => '(o e o))
(check ($ '(1 2 3 4) :filter (<= _ 3)) => '(1 2 3))
(check ($ '(1 2 3 4) :count (odd? _)) => 2)

;; === vector 与 _ 组合 ===
(check ($ #(1 2 3) :map (* _ 2)) => #(2 4 6))
(check ($ #(1 2 3 4) :filter (odd? _)) => #(1 3))
(check ($ #(1 2 3 4 5) :filter (<= _ 3) :map (* _ 10) :sum) => 60)

;; === 用户环境闭包捕获 ===
(let ((base 100))
  (check ($ '(1 2 3) :map (+ base _)) => '(101 102 103))
) ;let

;; === 卫生性：调用点 shadow + 不影响展开 ===
(let ((map (lambda args 'hijacked)))
  (check ($ '(1 2 3) :map (+ _ 1)) => '(2 3 4))
) ;let
