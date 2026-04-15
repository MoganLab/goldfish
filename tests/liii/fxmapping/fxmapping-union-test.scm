(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping-union
;; 并集操作。
;;
;; 语法
;; ----
;; (fxmapping-union fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; fxmap1, fxmap2, ... : fxmapping
;; 要合并的映射。
;;
;; 返回值
;; -----
;; 返回包含所有映射中所有键的新 fxmapping。
;; 对于重复键，后面的映射的值优先。
;;
(let ((union (fxmapping-union (fxmapping 0 'a 1 'b)
               (fxmapping 1 'B 2 'c)
             ) ;fxmapping-union
      ) ;union
     ) ;
  (check (fxmapping-ref union
           0
           (lambda () 'not-found)
         ) ;fxmapping-ref
    =>
    'a
  ) ;check
  (check (fxmapping-ref union
           1
           (lambda () 'not-found)
         ) ;fxmapping-ref
    =>
    'B
  ) ;check
  (check (fxmapping-ref union
           2
           (lambda () 'not-found)
         ) ;fxmapping-ref
    =>
    'c
  ) ;check
) ;let

(check-report)
