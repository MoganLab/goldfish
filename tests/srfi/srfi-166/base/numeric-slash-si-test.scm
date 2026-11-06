;; numeric/si
;; SI 词头数值组合子：用 k、M、G 等词头压缩大数。
;;
;; 语法
;; ----
;; (numeric/si n [base [separator]])
;;
;; 参数
;; ----
;; n : number?
;; 待格式化的数值。
;;
;; base : exact-integer?
;; 可选，词头的幂底数，默认 10。
;;
;; separator : string?
;; 可选，数值与词头之间的分隔串，默认为空。
;;
;; 返回值
;; ----
;; format?
;; 输出压缩后数值的格式化组合子。
;;
;; 说明
;; ----
;; 结果保留一位小数并按四舍五入压缩量级。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (numeric/si 608)) => "608")
(check (show #f (numeric/si 1536)) => "1.5k")
(check (show #f (numeric/si 3986000)) => "4M")
(check (show #f (numeric/si 3986 10 "_")) => "4_G")

(check-report)
