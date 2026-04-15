(import (liii check)
  (liii bag)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; bag<=? 函数测试
;;
;; 语法
;; ----
;; (bag<=? bag1 bag2 ...)
;;
;; 参数
;; ----
;; bag1, bag2 ... : bag
;; 参与比较的 bag。
;;
;; 返回值
;; -----
;; 如果 bag1 是 bag2 的子集（允许相等），返回 #t；否则返回 #f。

(let ((b1 (bag 1 1 2)) (b2 (bag 1 1 2 2)))
  (check-true (bag<=? b1 b2))
  (check-false (bag<=? b2 b1))
) ;let

(check-report)
