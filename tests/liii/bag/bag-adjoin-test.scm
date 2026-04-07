(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; bag-adjoin 函数测试
;;
;; 语法
;; ----
;; (bag-adjoin bag element ...)
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
;; 返回新的 bag，原 bag 保持不变（非破坏性）。

(let ((b (bag 1 2 2)))
  (define b2 (bag-adjoin b 2 3))
  (check (bag-size b) => 3)
  (check (bag-size b2) => 5)
  (check (bag-count (lambda (x) (= x 2)) b2) => 3)
) ;let
(check-catch 'type-error (bag-adjoin "not a bag" 1))

(check-report)
