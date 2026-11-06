;; split-pair-list
;; 将一个列表拆分为前缀元素列表与尾部。
;;
;; 语法
;; ----
;; (split-pair-list datum)
;;
;; 参数
;; ----
;; datum : list?
;; 待拆分的列表。
;;
;; 返回值
;; ----
;; 两个值：元素列表与尾部。
;; 元素列表是 list?，尾部在真列表时为 '()，在点对列表时为最后一个非列表元素。
;;
;; 说明
;; ----
;; 该函数以两个值的形式返回结果，需要配合 `call-with-values` 使用。

(import (liii check))
(import (liii goldfmt scan normalize))

(check-set-mode! 'report-failed)

(call-with-values (lambda () (split-pair-list '(a b c)))
  (lambda (elems tail) (check elems => '(a b c)) (check tail => '()))
) ;call-with-values

;; split-pair-list: 点对尾
(call-with-values (lambda () (split-pair-list '(a b . c)))
  (lambda (elems tail) (check elems => '(a b)) (check tail => 'c))
) ;call-with-values

;; split-pair-list: 空表
(call-with-values (lambda () (split-pair-list '()))
  (lambda (elems tail) (check elems => '()) (check tail => '()))
) ;call-with-values

(check-report)
