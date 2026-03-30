(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag-delete 函数测试
;;
;; 语法
;; ----
;; (bag-delete bag element ...)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; element ... : any
;; 要删除的元素，每个元素只删除一个实例。
;;
;; 返回值
;; -----
;; 返回新的 bag，原 bag 保持不变（非破坏性）。

(let ((b (bag 1 2 2 3)))
  (define b2 (bag-delete b 2 3))
  (check (bag-size b) => 4)
  (check (bag-size b2) => 2)
  (check (bag-count (lambda (x) (= x 2)) b2) => 1))

(check-report)
