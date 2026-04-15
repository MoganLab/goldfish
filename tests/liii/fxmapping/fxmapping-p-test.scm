(import (liii check) (liii fxmapping))

(check-set-mode! 'report-failed)

;; fxmapping?
;; 检查对象是否为整数映射（fxmapping）。
;;
;; 语法
;; ----
;; (fxmapping? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 fxmapping，返回 #t；否则返回 #f。
;;
(check-true (fxmapping? (fxmapping 0 'a))
) ;check-true
(check-false (fxmapping? '()))
(check-false (fxmapping? 42))

(check-report)
