(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-remove-front!
;; 从可变长向量前端移除元素。
;;
;; 语法
;; ----
;; (flexvector-remove-front! fv)
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove-front! fv) => 'a)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 0) => 'b))

(check-report)
