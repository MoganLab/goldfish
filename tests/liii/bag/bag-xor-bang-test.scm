(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; bag-xor! 函数测试
;;
;; 语法
;; ----
;; (bag-xor! bag1 bag2)
;;
;; 参数
;; ----
;; bag1, bag2 : bag
;; 参与运算的 bag（仅支持两个）。
;;
;; 返回值
;; -----
;; 返回修改后的 bag1（破坏性版本）。

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c)))
  (bag-xor! b1 b2)
  (check (bag-count (lambda (x) (eq? x 'a)) b1) => 1)
  (check (bag-count (lambda (x) (eq? x 'b)) b1) => 1)
  (check (bag-count (lambda (x) (eq? x 'c)) b1) => 1))

(check-report)
