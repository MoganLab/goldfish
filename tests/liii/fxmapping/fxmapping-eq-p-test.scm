(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping=?
;; 比较多个映射是否相等。
;;
;; 语法
;; ----
;; (fxmapping=? comparator fxmap1 fxmap2 ...)
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
;; 如果所有映射包含相同的键，且对应值通过 comparator 比较相等，返回 #t；
;; 否则返回 #f。
;;
(check-true (fxmapping=? eqv?
              (fxmapping 0 'a)
              (fxmapping 0 'a)
            ) ;fxmapping=?
) ;check-true
(check-false (fxmapping=? eqv?
               (fxmapping 0 'a)
               (fxmapping 0 'b)
             ) ;fxmapping=?
) ;check-false
(check-true (fxmapping=? eqv?
              (fxmapping 0 'a)
              (fxmapping 0 'a)
              (fxmapping 0 'a)
            ) ;fxmapping=?
) ;check-true

(check-report)
