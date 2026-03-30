(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-remove!
;; 从可变长向量中移除元素。
;;
;; 语法
;; ----
;; (flexvector-remove! fv index)
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 1) => 'b)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 1) => 'c))

(check-report)
