(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping>?
;; 检查映射1是否为映射2的真超集。
;;
;; 语法
;; ----
;; (fxmapping>? comparator fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; comparator : comparator
;; 值比较器。
;;
;; fxmap1, fxmap2, ... : fxmapping
;; 要比较的映射。
;;
;; 返回值
;; -----
;; 如果 fxmap1 是 fxmap2 的真超集，返回 #t；否则返回 #f。
;;
(check-true (fxmapping>? eqv? (fxmapping 0 'a) (fxmapping)))
(check-true (fxmapping>? eqv? (fxmapping 0 'a 1 'b) (fxmapping 0 'a)))
(check-false (fxmapping>? eqv? (fxmapping 0 'a) (fxmapping 0 'a)))
(check-false (fxmapping>? eqv? (fxmapping 0 'a) (fxmapping 0 'a 1 'b)))

(check-report)
