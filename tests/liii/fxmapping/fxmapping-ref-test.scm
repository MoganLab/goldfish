(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-ref
;; 获取指定键关联的值。
;;
;; 语法
;; ----
;; (fxmapping-ref fxmap key)
;; (fxmapping-ref fxmap key failure)
;; (fxmapping-ref fxmap key failure success)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要查找的键。
;;
;; failure : procedure (可选)
;; 键不存在时调用的无参过程，默认抛出错误。
;;
;; success : procedure (可选)
;; 键存在时调用的单参过程，接收值并返回结果，默认为 values。
;;
;; 返回值
;; -----
;; 如果键存在，返回关联值（或 success 的结果）；
;; 如果键不存在且提供了 failure，返回 failure 的结果；
;; 否则抛出错误。
;;
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 1 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 2 (lambda () 'not-found)) => 'not-found)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'fail) (lambda (v) (list 'found v))) => '(found a))

(check-report)
