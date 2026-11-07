;; max-fuzzy-edit-distance
;; 模糊匹配允许的最大编辑距离常量。
;;
;; 语法
;; ----
;; max-fuzzy-edit-distance
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; integer?，最大编辑距离。

(import (scheme base) (liii check) (liii golddoc fuzzy))

(check-set-mode! 'report-failed)

;; 常量值断言
(check max-fuzzy-edit-distance => 2)

(check-report)
