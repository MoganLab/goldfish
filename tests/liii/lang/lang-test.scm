;; (liii lang) 模块横切测试
;; $ 语法糖的卫生性、未命中报错、链式组合与裸值返回。

(import (liii check) (liii lang) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; === ($ x) 返回裸值，不装箱 ===
(check ($ '(1 2 3)) => '(1 2 3))
(check ($ 42) => 42)

;; === 链尾天然是裸值，无 :collect ===
(check ($ '(1 2 3 4 5) :filter odd? :map (lambda (x) (* x x))) => '(1 9 25))

;; === `_` 占位符自动提升 ===
(check ($ '(1 2 3) :map (+ _ 2)) => '(3 4 5))
(check ($ '(1 2 3) :map (- 10 _)) => '(9 8 7))
(check ($ '(1 2 3) :map (+ (* _ 2) 1)) => '(3 5 7))
(check ($ '(1 2 3 4) :filter (<= _ 3)) => '(1 2 3))
(check ($ '(1 2 3) :map (if (odd? _) 'o 'e)) => '(o e o))
(check ($ '(1 2 3 4 5) :filter (<= _ 3) :map (* _ 10) :sum) => 60)
(check ($ '(1 2 3) :map _) => '(1 2 3))

;; === 卫生性：调用点 shadow 不影响展开 ===
(let ((map (lambda args 'hijacked)))
  (check ($ '(1 2 3) :map (lambda (x) (+ x 1))) => '(2 3 4))
) ;let

;; === 用户环境闭包 ===
(let ((n 10))
  (check ($ '(1 2 3) :map (lambda (x) (+ x n))) => '(11 12 13))
) ;let

;; === 未命中算子 → 人性化报错 ===
(check-catch 'value-error ($ '(1 2 3) :mk-string ","))
(check-catch 'value-error ($ '(1 2 3) :collect))
(check-catch 'value-error ($ '(1 2 3) :flat-map (lambda (x) (list x))))
