(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag-adjoin! 函数测试
;;
;; 语法
;; ----
;; (bag-adjoin! bag element ...)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; element ... : any
;; 要添加的元素（可重复）。
;;
;; 返回值
;; -----
;; 就地修改原 bag，并返回修改后的 bag（破坏性）。

(let ((b (bag 1 2 2)))
  (bag-adjoin! b 2 3)
  (check (bag-size b) => 5))
(check-catch 'type-error (bag-adjoin! "not a bag" 1))

(check-report)
