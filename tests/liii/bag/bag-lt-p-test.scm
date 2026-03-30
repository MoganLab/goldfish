(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag<? 函数测试
;;
;; 语法
;; ----
;; (bag<? bag1 bag2 ...)
;;
;; 参数
;; ----
;; bag1, bag2 ... : bag
;; 参与比较的 bag。
;;
;; 返回值
;; -----
;; 如果 bag1 是 bag2 的真子集（bag1 的所有元素都在 bag2 中，但 bag2 包含更多元素），
;; 返回 #t；否则返回 #f。

(let ((b1 (bag 1 1 2))
      (b2 (bag 1 1 2 2)))
  (check-true (bag<? b1 b2))
  (check-false (bag<? b1 b1)))

(check-report)
