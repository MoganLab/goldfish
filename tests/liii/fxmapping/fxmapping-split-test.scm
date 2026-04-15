(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-split
;; 按键分割映射。
;;
;; 语法
;; ----
;; (fxmapping-split fxmap key)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 分割点。
;;
;; 返回值
;; -----
;; 返回两个值：键小于 key 的映射，和键大于等于 key 的映射。
;;
(let-values (((low high)
              (fxmapping-split (fxmapping 0 'a 1 'b 2 'c 3 'd)
                2
              ) ;fxmapping-split
             ) ;
            ) ;
  (check-true (fxmapping-contains? low 0))
  (check-true (fxmapping-contains? low 1))
  (check-false (fxmapping-contains? low 2)
  ) ;check-false
  (check-true (fxmapping-contains? high 2)
  ) ;check-true
  (check-true (fxmapping-contains? high 3)
  ) ;check-true
  (check-false (fxmapping-contains? high 1)
  ) ;check-false
) ;let-values

(check-report)
