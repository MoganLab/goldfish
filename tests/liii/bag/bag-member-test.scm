(import (liii check)
  (liii bag)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

;; bag-member 函数测试
;;
;; 语法
;; ----
;; (bag-member bag element default)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; element : any
;; 要查找的元素。
;;
;; default : any
;; 未找到时返回的默认值。
;;
;; 返回值
;; -----
;; 如果 bag 中存在与 element 等价的元素，返回该元素；否则返回 default。

(check (bag-member b-1-2 2 #f) => 2)
(check (bag-member b-1-2 9 'missing)
  =>
  'missing
) ;check
(check-catch 'type-error
  (bag-member "not a bag" 1 #f)
) ;check-catch
(check (bag-member b-empty 1 'none)
  =>
  'none
) ;check

(check-report)
