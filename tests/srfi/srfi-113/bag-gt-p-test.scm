;; bag>?
;; 如果 bag1 是 bag2 的真超集，返回 #t；否则返回 #f。
;;
;; 语法
;; ----
;; (bag>? bag1 bag2 ...)
;;
;; 参数
;; ----
;; bag1, bag2 ... : bag
;; 参与比较的 bag。
;;
;; 返回值
;; ----
;; 如果 bag1 是 bag2 的真超集，返回 #t；否则返回 #f。

(import (liii check) (liii bag) (liii error))

(check-set-mode! 'report-failed)


(let ((b1 (bag 1 1 2)) (b2 (bag 1 1 2 2)))
  (check-true (bag>? b2 b1))
) ;let

(check-report)
