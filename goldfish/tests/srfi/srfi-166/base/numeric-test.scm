;; numeric
;; 数值格式化组合子：控制基数、精度、千分位与小数点对齐。
;;
;; 语法
;; ----
;; (numeric n [radix [precision [sign-rule [comma-rule [comma-sep
;;           [decimal-sep]]]]]])
;; (numeric/comma n [comma-rule])
;; (numeric/si n [base [separator]])
;; (numeric/fitted width n . rest)
;;
;; 参数
;; ----
;; n : number?
;; 待格式化的数值。
;;
;; radix : exact-integer?
;; 可选，进制基数，默认取状态变量 radix（10）。
;;
;; precision : exact-integer?
;; 可选，小数位数，默认取状态变量 precision。
;;
;; comma-rule : exact-integer?
;; 可选，每几位插入一个千分位分隔符，0 表示不分隔。
;;
;; 返回值
;; ----
;; format?
;; 输出格式化数字字符串的组合子。
;;
;; 说明
;; ----
;; 省略参数时全部取对应的格式化状态变量，因此可以直接
;; 用 with 调整 radix、precision、comma-rule 等；numeric/comma
;; 只加千分位，numeric/si 用 k、M 等 SI 词头压缩量级，
;; numeric/fitted 在 numeric 之上再定宽输出。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; 默认规则：数字按 display 语义输出
(check (show #f -1) => "-1")
(check (show #f 0) => "0")
(check (show #f (numeric 100)) => "100")

;; 基数与精度
(check (show #f (numeric 2 2)) => "10")
(check (show #f (numeric 9 2)) => "1001")
(check (show #f (numeric 9/8 10 2)) => "1.12")
(check (show #f (numeric 9/8 10 4)) => "1.1250")

;; 由状态变量控制的基数与精度
(check
  (show #f (with ((radix 16)) 57005))
  =>
  "#xdead"
) ;check
(check
  (show #f (with ((precision 2)) 3.14159))
  =>
  "3.14"
) ;check
(check
  (show #f (with ((precision 2)) 3.0))
  =>
  "3.00"
) ;check

;; 千分位与小数点对齐
(check
  (show #f (with ((comma-rule 3)) (numeric 299792458)))
  =>
  "299,792,458"
) ;check
(check
  (show #f (with ((comma-rule 3) (comma-sep #\.)) (numeric 299792458)))
  =>
  "299.792.458"
) ;check
(check
  (show #f (with ((decimal-align 5)) (numeric 3.14159)))
  =>
  "   3.14159"
) ;check

(check-report)
