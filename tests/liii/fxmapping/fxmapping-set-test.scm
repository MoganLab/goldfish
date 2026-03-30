(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-set
;; 设置键值对，覆盖已存在的键。
;;
;; 语法
;; ----
;; (fxmapping-set fxmap key value ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要设置的键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，包含原映射的所有键值对以及新设置的键值对。
;; 如果键已存在，新值会覆盖原值。
;;
(check (fxmapping-ref (fxmapping-set (fxmapping 0 'a) 0 'b) 0 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping-set (fxmapping 0 'a) 1 'b) 1 (lambda () 'not-found)) => 'b)

(check-report)
