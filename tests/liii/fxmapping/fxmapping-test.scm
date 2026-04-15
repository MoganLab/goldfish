(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping
;; 创建一个新的整数映射（fxmapping）。
;;
;; 语法
;; ----
;; (fxmapping key value ...)
;;
;; 参数
;; ----
;; key : exact-integer
;; 整数键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回包含指定键值对的新 fxmapping。
;;
(check-true (fxmapping? (fxmapping 0 'a 1 'b))
) ;check-true
(check (fxmapping-ref (fxmapping 0 'a 1 'b)
         0
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'a
) ;check
(check (fxmapping-ref (fxmapping 0 'a 1 'b)
         1
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'b
) ;check
(check (fxmapping-ref (fxmapping 0 'a 1 'b)
         2
         (lambda () 'not-found)
       ) ;fxmapping-ref
  =>
  'not-found
) ;check
(check-true (fxmapping-empty? (fxmapping))
) ;check-true

(check-report)
