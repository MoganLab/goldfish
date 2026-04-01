(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-reverse-copy
;; 创建可变长向量的反向副本。
;;
;; 语法
;; ----
;; (flexvector-reverse-copy fv)
;; (flexvector-reverse-copy fv start)
;; (flexvector-reverse-copy fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;; 源向量。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引，默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引，默认为向量长度。
;;
;; 返回值
;; ----
;; flexvector
;; 反向顺序的新向量。
;;
;; 描述
;; ----
;; 创建一个新向量，包含源向量指定范围的反向元素。

(let ((fv (flexvector 1 2 3)))
  (let ((rev (flexvector-reverse-copy fv)))
    (check (flexvector->list rev) => '(3 2 1))
    (check (flexvector->list fv) => '(1 2 3))
  ) ;let
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e)))
  (check (flexvector->vector (flexvector-reverse-copy fv 1 4)) => #(d c b))
) ;let

(check-report)
