(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; bag-xor 函数测试
;;
;; 语法
;; ----
;; (bag-xor bag1 bag2)
;;
;; 参数
;; ----
;; bag1, bag2 : bag
;; 参与运算的 bag（仅支持两个）。
;;
;; 返回值
;; -----
;; 返回新的 bag，不修改原 bag（非破坏性版本）。

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c))
       (x (bag-xor b1 b2)))
  (check (bag-count (lambda (x) (eq? x 'a)) x) => 1)
  (check (bag-count (lambda (x) (eq? x 'b)) x) => 1)
  (check (bag-count (lambda (x) (eq? x 'c)) x) => 1)
) ;let*

(check-report)
