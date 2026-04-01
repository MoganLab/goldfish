(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-remove-front!
;; 从可变长向量头部移除元素。
;;
;; 语法
;; ----
;; (flexvector-remove-front! fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; 返回值
;; ----
;; any
;; 返回被移除的元素。
;;
;; 错误处理
;; ----
;; bounds-error
;; 当向量为空时抛出。

(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove-front! fv) => 'a)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector->list fv) => '(b c))
) ;let

(let ((fv (flexvector 'x)))
  (check (flexvector-remove-front! fv) => 'x)
  (check (flexvector-empty? fv) => #t)
) ;let

(check-report)
