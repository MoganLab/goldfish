(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector?
;; 检查对象是否为可变长向量（flexvector）。时间复杂度 O(1)。
;;
;; 语法
;; ----
;; (flexvector? obj)
;;
;; 参数
;; ----
;; obj : any
;;   要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 flexvector，返回 #t；否则返回 #f。
;;
;; 示例
;; ----
;; (flexvector? (flexvector))           => #t
;; (flexvector? (flexvector 1 2 3))     => #t
;; (flexvector? '())                    => #f
;; (flexvector? #())                    => #f  ; 普通向量不是 flexvector

;; flexvector 返回 #t
(check-true (flexvector? (flexvector)))
(check-true (flexvector? (flexvector 1 2 3)))
(check-true (flexvector? (make-flexvector 10)))
(check-true (flexvector? (list->flexvector '(a b c))))

;; 非 flexvector 返回 #f
(check-false (flexvector? '()))
(check-false (flexvector? #()))           ; 普通向量
(check-false (flexvector? "not a flexvector"))
(check-false (flexvector? 42))
(check-false (flexvector? #t))
(check-false (flexvector? 'symbol))
(check-false (flexvector? '(1 2 3)))      ; 列表

;; 修改后仍然是 flexvector
(let ((fv (flexvector 1 2)))
  (check-true (flexvector? fv))
  (flexvector-add-back! fv 3)
  (check-true (flexvector? fv)))

(check-report)
