;; %syntax-case-fold-right
;; 右折叠函数，用于展开模板中带省略号的重复结构。
;;
;; 语法
;; ----
;; (%syntax-case-fold-right f init . lists)
;;
;; 参数
;; ----
;; f : procedure
;; 结合过程。
;;
;; init : any
;; 初始累积值。
;;
;; lists : list ...
;; 待折叠的列表序列。
;;
;; 返回值
;; -----
;; any
;; 折叠计算结果。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 单列表右折叠
(check (%syntax-case-fold-right cons '() '(1 2 3)) => '(1 2 3))

(check-report)
