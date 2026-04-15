(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-reverse!
;; 原地反转 flexvector。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (flexvector-reverse! fv)
;; (flexvector-reverse! fv start)
;; (flexvector-reverse! fv start end)
;;
;; 参数
;; ----
;; fv : flexvector
;;   目标向量，会被修改。
;;
;; start : exact-nonnegative-integer (可选，默认 0)
;;   起始索引（包含）。
;;
;; end : exact-nonnegative-integer (可选，默认长度)
;;   结束索引（不包含）。
;;
;; 返回值
;; -----
;; 返回值未指定。
;;
;; 副作用
;; -----
;; 反转 fv 中指定范围的元素顺序。
;;
;; 另见
;; ----
;; flexvector-reverse-copy - 非破坏性反转


;; 基本反转
(let ((fv (flexvector 1 2 3)))
  (flexvector-reverse! fv)
  (check (flexvector->list fv)
    =>
    '(3 2 1)
  ) ;check
) ;let


;; 反转后再反转
(let ((fv (flexvector 'a 'b 'c 'd)))
  (flexvector-reverse! fv)
  (flexvector-reverse! fv)
  (check (flexvector->list fv)
    =>
    '(a b c d)
  ) ;check
) ;let


;; 空向量（无变化）
(let ((fv (flexvector)))
  (flexvector-reverse! fv)
  (check (flexvector-empty? fv) => #t)
) ;let


;; 单元素（无变化）
(let ((fv (flexvector 'only)))
  (flexvector-reverse! fv)
  (check (flexvector->list fv) => '(only))
) ;let


;; 双元素
(let ((fv (flexvector 'a 'b)))
  (flexvector-reverse! fv)
  (check (flexvector->list fv) => '(b a))
) ;let


;; 反转区间 [start, end)
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-reverse! fv 1 4)
  (check (flexvector->list fv)
    =>
    '(1 4 3 2 5)
  ) ;check
) ;let


;; 反转前缀
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-reverse! fv 0 3)
  (check (flexvector->list fv)
    =>
    '(3 2 1 4 5)
  ) ;check
) ;let


;; 反转后缀
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-reverse! fv 2)
  (check (flexvector->list fv)
    =>
    '(1 2 5 4 3)
  ) ;check
) ;let


;; 空区间
(let ((fv (flexvector 1 2 3)))
  (flexvector-reverse! fv 1 1)
  (check (flexvector->list fv)
    =>
    '(1 2 3)
  ) ;check
) ;let


(check-report)
