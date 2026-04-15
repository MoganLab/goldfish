(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-pop-max
;; 弹出键最大的键值对。
;;
;; 语法
;; ----
;; (fxmapping-pop-max fxmap)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射，必须非空。
;;
;; 返回值
;; -----
;; 返回三个值：最大的键、关联的值、以及不包含该键值对的新映射。
;;
(let-values (((k v m)
              (fxmapping-pop-max (fxmapping 0 'a 1 'b)
              ) ;fxmapping-pop-max
             ) ;
            ) ;
  (check k => 1)
  (check v => 'b)
  (check-false (fxmapping-contains? m 1))
) ;let-values

(check-report)
