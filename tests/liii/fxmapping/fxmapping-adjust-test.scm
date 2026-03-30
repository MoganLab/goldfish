(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-adjust
;; 调整指定键的值。
;;
;; 语法
;; ----
;; (fxmapping-adjust fxmap key proc)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要调整的键。
;;
;; proc : procedure
;; 单参过程，接收原值并返回新值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，指定键的值已调整。
;; 如果键不存在，返回原映射。
;;
(check (fxmapping-ref (fxmapping-adjust (fxmapping 0 10) 0 (lambda (v) (* v 2))) 0 (lambda () 'not-found)) => 20)

(check-report)
