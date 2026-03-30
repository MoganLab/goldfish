(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-delete
;; 删除指定键。
;;
;; 语法
;; ----
;; (fxmapping-delete fxmap key ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; key : exact-integer
;; 要删除的键。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping，不包含指定的键。
;;
(check-false (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 0) 0))
(check-true (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 0) 1))
(check-true (fxmapping-contains? (fxmapping-delete (fxmapping 0 'a 1 'b) 2) 0))

(check-report)
