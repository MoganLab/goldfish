(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-ref
;; 按索引访问向量中的元素。
;;
;; 语法
;; ----
;; (vector-ref vec k)
;;
;; 参数
;; ----
;; vec : vector?
;; 要读取的向量。
;;
;; k : integer?
;; 要访问的索引位置，必须在有效范围内。
;;
;; 返回值
;; ----
;; any?
;; 向量中索引k对应的元素。
;;
;; 注意
;; ----
;; 向量索引从0开始计数。
;;
;; 示例
;; ----
;; (vector-ref #(a b c) 1) => b
;; (vector-ref #(42) 0) => 42
;;
;; 错误处理
;; ----
;; out-of-range 当k超出向量边界时
;; wrong-type-arg 当vec不是向量，或k不是整数时


(let ((v #(1 2 3)))
  (check (vector-ref v 0) => 1)
  (check (vector-ref v 1) => 2)
  (check (vector-ref v 2) => 3)
) ;let


(let ((v #(a b c d)))
  (check (vector-ref v 0) => 'a)
  (check (vector-ref v 3) => 'd)
) ;let


(check-catch 'out-of-range
  (vector-ref #() 0)
) ;check-catch
(check (vector-ref #(42) 0) => 42)


(let ((v #(1 2 3)))
  (check-catch 'out-of-range
    (vector-ref v -1)
  ) ;check-catch
  (check-catch 'out-of-range
    (vector-ref v 3)
  ) ;check-catch
) ;let


(let ((v #(1 2.5 "hello" (#_quote symbol) #\c #t #f)
      ) ;v
     ) ;
  (check (vector-ref v 0) => 1)
  (check (vector-ref v 2) => "hello")
  (check (vector-ref v 4) => #\c)
  (check (vector-ref v 6) => #f)
) ;let


(check-report)
