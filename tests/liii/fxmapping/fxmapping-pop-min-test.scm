(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-pop-min
;; 弹出键最小的键值对。
;;
;; 语法
;; ----
;; (fxmapping-pop-min fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回三个值：最小的键、关联的值、以及不包含该键值对的新映射。
;;
(let-values (((k v m)
              (fxmapping-pop-min (fxmapping 0 'a 1 'b)
              ) ;fxmapping-pop-min
             ) ;
            ) ;
  (check k => 0)
  (check v => 'a)
  (check-false (fxmapping-contains? m 0))
) ;let-values

(check-report)
