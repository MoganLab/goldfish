(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-intersection
;; 交集操作。
;;
;; 语法
;; ----
;; (fxmapping-intersection fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1, fxmap2, ... : fxmapping
;; 要求交集的映射。
;;
;; 返回值
;; -----
;; 返回只包含在所有映射中都存在的键的新 fxmapping。
;; 对于重复键，后面的映射的值优先。
;;
(let ((intersection (fxmapping-intersection (fxmapping 0 'a 1 'b 2 'c)
                      (fxmapping 1 'B 2 'C 3 'd)
                    ) ;fxmapping-intersection
      ) ;intersection
     ) ;
  (check-false (fxmapping-contains? intersection 0)
  ) ;check-false
  (check (fxmapping-ref intersection
           1
           (lambda () 'not-found)
         ) ;fxmapping-ref
    =>
    'B
  ) ;check
  (check (fxmapping-ref intersection
           2
           (lambda () 'not-found)
         ) ;fxmapping-ref
    =>
    'C
  ) ;check
) ;let

(check-report)
