(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; list->flexvector
;; 将列表转换为可变长向量。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (list->flexvector list)
;;
;; 参数
;; ----
;; list : list
;;   源列表。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，元素顺序与列表相同。
;;
;; 另见
;; ----
;; flexvector->list - 向量转列表


;; 基本转换
(check (flexvector->list (list->flexvector '(a b c))
       ) ;flexvector->list
  =>
  '(a b c)
) ;check


;; 空列表
(check (flexvector->list (list->flexvector '())
       ) ;flexvector->list
  =>
  '()
) ;check


;; 单元素
(let ((fv (list->flexvector '(only))))
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'only)
) ;let


;; 大量元素（测试内部容量分配）
(let* ((lst (iota 20))
       (fv (list->flexvector lst))
      ) ;
  (check (flexvector-length fv) => 20)
  (check (flexvector->list fv) => lst)
) ;let*


;; 往返测试
(let ((lst '(1 2 3 4 5)))
  (check (flexvector->list (list->flexvector lst)
         ) ;flexvector->list
    =>
    lst
  ) ;check
) ;let


(check-report)
