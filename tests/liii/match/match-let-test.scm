(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match-let
;; 并行匹配并绑定变量，类似于 let，但在绑定位置支持任意模式解构。
;; 亦支持命名循环（named let）形式。
;;
;; 语法
;; ----
;; (match-let ((pattern expr) ...) body ...)
;; (match-let loop ((pattern expr) ...) body ...)
;;
;; 参数
;; ----
;; loop : identifier (可选)
;; 命名循环的过程名。
;;
;; pattern : pattern
;; 用于解构对应 expr 值的模式。
;;
;; expr : any
;; 待求值并进行模式解构的表达式。
;;
;; body : any
;; 在绑定作用域内执行的主体表达式。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

;; 基础多变量并行解构
(check
  (match-let (((x y) '(1 2)) ((z) '(3))) (+ x y z))
  =>
  6
) ;check

;; 列表解构与点对解构混合
(check
  (match-let (((a . b) '(1 . 2)) (#(c d) '#(3 4))) (+ a b c d))
  =>
  10
) ;check

;; 命名 match-let (递归循环)
(check
  (match-let loop
   (((x . rest) '(1 2 3 4)) (sum 0))
   (if (null? rest) (+ sum x) (loop rest (+ sum x)))
  ) ;match-let
  =>
  10
) ;check

;; 匹配失败抛出 match-error
(check-catch 'match-error (match-let (((x y) '(1))) (+ x y)))

(check-report)
