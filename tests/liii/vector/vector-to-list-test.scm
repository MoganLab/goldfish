(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector->list
;; 将向量转换为列表。
;;
;; 语法
;; ----
;; (vector->list vec)
;; (vector->list vec start)
;; (vector->list vec start end)
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
;; 由指定区间元素构成的新列表。
;;
;; 注意
;; ----
;; 当start和end相等时，返回空列表。
;;
;; 示例
;; ----
;; (vector->list #(0 1 2 3) 1 3) => '(1 2)
;; (vector->list #()) => '()
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出向量边界或start大于end时
;; wrong-type-arg 当vec不是向量，或start/end不是整数时


(check (vector->list #()) => ())
(check (vector->list #(a b c))
  =>
  '(a b c)
) ;check
(check (vector->list #(1 2 3))
  =>
  '(1 2 3)
) ;check
(check (vector->list #(42)) => '(42))
(check (vector->list #(a)) => '(a))
(check (vector->list #(1 2.5 "hello" (#_quote symbol) #\c #t #f)
       ) ;vector->list
  =>
  '(1 2.5 "hello" (#_quote symbol) #\c #t #f)
) ;check
(check (vector->list #((1 2) (3 4)))
  =>
  '((1 2) (3 4))
) ;check


(check (vector->list #(0 1 2 3) 1)
  =>
  '(1 2 3)
) ;check
(check (vector->list #(0 1 2 3) 2)
  =>
  '(2 3)
) ;check
(check (vector->list #(0 1 2 3) 3)
  =>
  '(3)
) ;check
(check (vector->list #(0 1 2 3) 4)
  =>
  ()
) ;check


(check (vector->list #(0 1 2 3) 1 3)
  =>
  '(1 2)
) ;check
(check (vector->list #(0 1 2 3) 0 4)
  =>
  '(0 1 2 3)
) ;check
(check (vector->list #(0 1 2 3) 1 1)
  =>
  ()
) ;check
(check (vector->list #(0 1 2 3) 2 4)
  =>
  '(2 3)
) ;check


(let ((v #(1 2 3)))
  (check-catch 'out-of-range
    (vector->list v -1)
  ) ;check-catch
  (check-catch 'out-of-range
    (vector->list v 4)
  ) ;check-catch
  (check-catch 'out-of-range
    (vector->list v 2 5)
  ) ;check-catch
  (check-catch 'out-of-range
    (vector->list v 3 2)
  ) ;check-catch
) ;let


(check-catch 'wrong-type-arg
  (vector->list 'not-a-vector)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector->list #(1 2 3) 'not-a-number)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector->list #(1 2 3) 0 'not-a-number)
) ;check-catch


(check-report)
