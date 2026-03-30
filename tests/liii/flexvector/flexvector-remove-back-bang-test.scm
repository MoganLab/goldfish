(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-remove-back!
;; 从可变长向量后端移除元素。
;;
;; 语法
;; ----
;; (flexvector-remove-back! fv)
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove-back! fv) => 'c)
  (check (flexvector-empty? fv) => #f)
  (flexvector-remove-back! fv)
  (flexvector-remove-back! fv)
  (check (flexvector-empty? fv) => #t))

(check-report)
