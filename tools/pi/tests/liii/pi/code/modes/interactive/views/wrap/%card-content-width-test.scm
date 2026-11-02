;; %card-content-width
;; 计算卡片正文可用宽度，扣除左右留白并至少保留 1 列。
;;
;; 语法
;; ----
;; (%card-content-width width left-pad right-pad)
;;
;; 参数
;; ----
;; width : integer?
;; 终端总宽度。
;; left-pad : integer?
;; 左侧留白列数。
;; right-pad : integer?
;; 右侧留白列数。
;;
;; 返回值
;; ----
;; integer?
;; 卡片正文最大可用列宽（>= 1）。

(import (scheme base) (liii check) (liii pi code modes interactive views wrap))

(check-set-mode! 'report-failed)

;; 1. 正常计算宽度
(check (%card-content-width 80 2 2) => 76)
(check (%card-content-width 40 1 1) => 38)

;; 2. 宽度过小时至少保底 1 列
(check (%card-content-width 5 4 4) => 1)
(check (%card-content-width 0 2 2) => 1)

(check-report)
