(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-max
;; 获取键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回两个值：最大的键和关联的值。
;;
(let-values (((k v) (fxmapping-max (fxmapping 0 'a 1 'b 2 'c))))
  (check k => 2)
  (check v => 'c)
) ;let-values

(check-report)
