(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-disjoint?
;; 检查两个映射是否不相交（没有共同键）。
;;
;; 语法
;; ----
;; (fxmapping-disjoint? fxmap1 fxmap2)
;;
;; 参数
;; ----
;; fxmap1, fxmap2 : fxmapping
;; 要比较的两个映射。
;;
;; 返回值
;; -----
;; 如果两个映射没有共同键，返回 #t；否则返回 #f。
;;
(check-true (fxmapping-disjoint? (fxmapping 0 'a) (fxmapping 1 'b)))
(check-false (fxmapping-disjoint? (fxmapping 0 'a) (fxmapping 0 'b)))
(check-true (fxmapping-disjoint? (fxmapping 0 'a 1 'b) (fxmapping 2 'c 3 'd)))

(check-report)
