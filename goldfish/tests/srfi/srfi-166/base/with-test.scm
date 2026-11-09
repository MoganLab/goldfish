;; with
;; 在局部作用域内绑定格式化状态变量的组合子。
;;
;; 语法
;; ----
;; (with ((var val) ...) fmt ...)
;;
;; 参数
;; ----
;; var : symbol?
;; 状态变量名，如 pad-char、ellipsis、radix、precision、comma-rule。
;;
;; val : any
;; 该变量在子组合子内生效的值。
;;
;; fmt : format?
;; 在绑定环境下求值的格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 仅在本层生效的组合子，外层状态变量不受影响。
;;
;; 说明
;; ----
;; with! 与之相对，会就地修改当前环境的变量值；
;; 读取变量值需要在 fn 的形参表中进行。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; 绑定只在内部生效
(check
  (show #f (with ((pad-char #\-)) (padded 4 "ab")) "ab")
  =>
  "--abab"
) ;check

;; 多层绑定按从外到内生效
(check
  (show #f (with ((precision 2)) (with ((precision 3)) 3.14159)))
  =>
  "3.142"
) ;check

;; 读取被绑定的值要用 fn 形参
(check
  (show #f (with ((row 3)) (fn (row) (each "r=" row))))
  =>
  "r=3"
) ;check

;; 空绑定等价于直接输出
(check (show #f (with () (each "x"))) => "x")

(check-report)
