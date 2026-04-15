(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-map
;; 映射函数转换所有值。
;;
;; 语法
;; ----
;; (fxmapping-map proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接收 key 和 value，返回新值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，所有值都经过 proc 转换。
;;
(check (fxmapping-ref (fxmapping-map (lambda (k v) (* v 10))
                        (fxmapping 0 1 1 2 2 3)
                      ) ;fxmapping-map
         0
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  10
) ;check
(check (fxmapping-ref (fxmapping-map (lambda (k v) (* v 10))
                        (fxmapping 0 1 1 2 2 3)
                      ) ;fxmapping-map
         1
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  20
) ;check

(check-report)
