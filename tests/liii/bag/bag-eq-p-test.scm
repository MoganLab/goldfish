(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag=? 函数测试
;;
;; 语法
;; ----
;; (bag=? bag1 bag2 ...)
;;
;; 参数
;; ----
;; bag1, bag2 ... : bag
;; 参与比较的 bag。
;;
;; 返回值
;; -----
;; 如果两个 bag 包含相同的元素（包括重复次数），返回 #t；否则返回 #f。

(let ((b1 (bag 1 1 2))
      (b2 (bag 1 1 2 2))
      (b3 (bag 1 1 2)))
  (check-true (bag=? b1 b3))
  (check-false (bag=? b1 b2)))

(check-report)
