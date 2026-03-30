(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-min
;; 获取键最小的键值对。
;;
;; 语法
;; ----
;; (fxmapping-min fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回两个值：最小的键和关联的值。
;;
(let-values (((k v) (fxmapping-min (fxmapping 0 'a 1 'b 2 'c))))
  (check k => 0)
  (check v => 'a)
) ;let-values

(check-report)
