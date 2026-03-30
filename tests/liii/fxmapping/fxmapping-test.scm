(import (liii check)
        (liii fxmapping)
) ;import

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
(check-true (fxmapping? (fxmapping 0 'a 1 'b)))
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 0 (lambda () 'not-found)) => 'a)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 1 (lambda () 'not-found)) => 'b)
(check (fxmapping-ref (fxmapping 0 'a 1 'b) 2 (lambda () 'not-found)) => 'not-found)
(check-true (fxmapping-empty? (fxmapping)))

(check-report)
