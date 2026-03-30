(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping->decreasing-alist
;; 转换为关联列表（降序）。
;;
;; 语法
;; ----
;; (fxmapping->decreasing-alist fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回按键降序排列的关联列表。
;;
(check (fxmapping->decreasing-alist (fxmapping 0 'a 1 'b)) => '((1 . b) (0 . a)))

(check-report)
