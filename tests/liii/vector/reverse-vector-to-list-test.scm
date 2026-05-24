(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; reverse-vector->list
;; 将向量转换为逆序列表。
;;
;; 语法
;; ----
;; (reverse-vector->list vec)
;; (reverse-vector->list vec start)
;; (reverse-vector->list vec start end)
;;
;; 参数
;; ----
;; vec : vector?
;; 要转换的向量。
;;
;; start : integer? 可选
;; 起始位置（包含），默认为0。
;;
;; end : integer? 可选
;; 结束位置（不包含），默认为向量长度。
;;
;; 返回值
;; ----
;; list
;; 由指定区间元素逆序构成的新列表。
;;
;; 注意
;; ----
;; 当start和end相等时，返回空列表。
;;
;; 示例
;; ----
;; (reverse-vector->list #(1 2 3)) => '(3 2 1)
;; (reverse-vector->list #(0 1 2 3) 1 3) => '(2 1)
;; (reverse-vector->list #()) => '()
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出向量边界或start大于end时
;; wrong-type-arg 当vec不是向量，或start/end不是整数时


(check (reverse-vector->list #()) => ())
(check (reverse-vector->list #(a b c)) => '(c b a))
(check (reverse-vector->list #(1 2 3)) => '(3 2 1))
(check (reverse-vector->list #(42)) => '(42))
(check (reverse-vector->list #(1 2.5 "hello" 'symbol #\c #t #f))
  =>
  '(#f #t #\c 'symbol "hello" 2.5 1)
) ;check
(check (reverse-vector->list #((1 2) (3 4))) => '((3 4) (1 2)))


(check (reverse-vector->list #(0 1 2 3) 1) => '(3 2 1))
(check (reverse-vector->list #(0 1 2 3) 2) => '(3 2))
(check (reverse-vector->list #(0 1 2 3) 3) => '(3))
(check (reverse-vector->list #(0 1 2 3) 4) => ())


(check (reverse-vector->list #(0 1 2 3) 1 3) => '(2 1))
(check (reverse-vector->list #(0 1 2 3) 0 4) => '(3 2 1 0))
(check (reverse-vector->list #(0 1 2 3) 1 1) => ())
(check (reverse-vector->list #(0 1 2 3) 2 4) => '(3 2))


(let ((v #(1 2 3)))
  (check-catch 'out-of-range (reverse-vector->list v -1))
  (check-catch 'out-of-range (reverse-vector->list v 4))
  (check-catch 'out-of-range (reverse-vector->list v 2 5))
  (check-catch 'out-of-range (reverse-vector->list v 3 2))
) ;let


(check-catch 'wrong-type-arg (reverse-vector->list 'not-a-vector))
(check-catch 'wrong-type-arg (reverse-vector->list #(1 2 3) 'not-a-number))
(check-catch 'wrong-type-arg (reverse-vector->list #(1 2 3) 0 'not-a-number))


(check-report)
