(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-add!
;; 向可变长向量中添加元素。
;;
;; 语法
;; ----
;; (flexvector-add! fv index element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 插入位置，必须满足 0 <= index <= (flexvector-length fv)。
;;
;; element ... : any
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector。
;;
;; 边界情况
;; ------
;; - index = 0: 在开头插入（等价于 flexvector-add-front!）
;; - index = len: 在末尾插入（等价于 flexvector-add-back!）
;; - index < 0 或 index > len: 非法，触发错误

(let ((fv (flexvector)))
  (flexvector-add! fv 0 'a)
  (check (flexvector-ref fv 0) => 'a))

(let ((fv (flexvector 'a 'c)))
  (flexvector-add! fv 1 'b)
  (check (flexvector-length fv) => 3)
  (check (flexvector->list fv) => '(a b c)))

(let ((fv (flexvector 'a 'b)))
  (flexvector-add! fv 2 'c)
  (check (flexvector-length fv) => 3)
  (check (flexvector->list fv) => '(a b c)))

(let ((fv (flexvector 'a)))
  (flexvector-add! fv 1 'b 'c 'd)
  (check (flexvector-length fv) => 4)
  (check (flexvector->list fv) => '(a b c d)))

;; 非法 index 测试：index > len
(check-catch 'value-error
  (let ((fv (flexvector 'a 'b 'c)))
    (flexvector-add! fv 5 'x)))

;; 非法 index 测试：index < 0
(check-catch 'value-error
  (let ((fv (flexvector 'a 'b 'c)))
    (flexvector-add! fv -1 'x)))

(check-report)
