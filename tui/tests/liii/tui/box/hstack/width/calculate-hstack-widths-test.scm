;; calculate-hstack-widths
;; 根据可用总宽、列数、间距与宽度规格计算水平栈各列分配的宽度列表。
;;
;; 语法
;; ----
;; (calculate-hstack-widths total-width num-cols spacing widths-spec)
;;
;; 参数
;; ----
;; total-width : integer?
;; 可用总宽度。
;;
;; num-cols : integer?
;; 列数。
;;
;; spacing : integer?
;; 列间距。
;;
;; widths-spec : any
;; 宽度规格：可为 'auto、数字列表、比例或混排列表。
;;
;; 返回值
;; ----
;; list?
;; 每列分配的精确整数宽度列表。

(import (scheme base) (liii check) (liii tui box hstack width))

(check-set-mode! 'report-failed)

;; 1. 均分（auto）
(check (calculate-hstack-widths 20 2 0 'auto) => '(10 10))
(check (calculate-hstack-widths 21 2 1 'auto) => '(10 10))

;; 2. 比例划分
(check (calculate-hstack-widths 100 2 0 '(1 3)) => '(25 75))

;; 3. 边界情况
(check (calculate-hstack-widths 10 0 0 'auto) => '())

(check-report)
