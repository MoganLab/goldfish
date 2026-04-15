(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-add!
;; 在 flexvector 的指定位置插入元素。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-add! fv index element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; index : exact-nonnegative-integer
;;   插入位置，必须满足 0 <= index <= (flexvector-length fv)。
;;   - index = 0: 在开头插入（等价于 flexvector-add-front!）
;;   - index = len: 在末尾插入（等价于 flexvector-add-back!）
;;
;; element ... : any
;;   要插入的一个或多个元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector（即输入的 fv）。
;;
;; 错误
;; ----
;; index < 0 或 index > (flexvector-length fv) 会抛出错误。
;;
;; 另见
;; ----
;; flexvector-add-front! - 头部插入
;; flexvector-add-back! - 尾部插入


;; 在开头插入
(let ((fv (flexvector)))
  (flexvector-add! fv 0 'a)
  (check (flexvector-ref fv 0) => 'a)
) ;let


;; 在中间插入
(let ((fv (flexvector 'a 'c)))
  (flexvector-add! fv 1 'b)
  (check (flexvector-length fv) => 3)
  (check (flexvector->list fv)
    =>
    '(a b c)
  ) ;check
) ;let


;; 在末尾插入
(let ((fv (flexvector 'a 'b)))
  (flexvector-add! fv 2 'c)
  (check (flexvector-length fv) => 3)
  (check (flexvector->list fv)
    =>
    '(a b c)
  ) ;check
) ;let


;; 插入多个元素
(let ((fv (flexvector 'a)))
  (flexvector-add! fv 1 'b 'c 'd)
  (check (flexvector-length fv) => 4)
  (check (flexvector->list fv)
    =>
    '(a b c d)
  ) ;check
) ;let


;; 在开头插入多个元素
(let ((fv (flexvector 'z)))
  (flexvector-add! fv 0 'a 'b 'c)
  (check (flexvector->list fv)
    =>
    '(a b c z)
  ) ;check
) ;let


;; 非法 index 测试：index > len
(check-catch 'value-error
  (let ((fv (flexvector 'a 'b 'c)))
    (flexvector-add! fv 5 'x)
  ) ;let
) ;check-catch


;; 非法 index 测试：index < 0
(check-catch 'value-error
  (let ((fv (flexvector 'a 'b 'c)))
    (flexvector-add! fv -1 'x)
  ) ;let
) ;check-catch


;; 返回值是原对象
(let ((fv (flexvector 1 2)))
  (check (eq? (flexvector-add! fv 1 'x) fv)
    =>
    #t
  ) ;check
) ;let


(check-report)
