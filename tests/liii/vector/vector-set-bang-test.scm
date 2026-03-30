(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; vector-set!
;; 修改向量中指定位置的元素。
;;
;; 语法
;; ----
;; (vector-set! vec k obj)
;;
;; 参数
;; ----
;; vec : vector?
;; 要修改的向量。
;;
;; k : integer?
;; 要写入的索引位置。
;;
;; obj : any?
;; 要设置的新值。
;;
;; 返回值
;; ----
;; any?
;; 实现相关的返回值，测试主要关注副作用结果。
;;
;; 注意
;; ----
;; 这是原地修改操作，会直接改变原向量内容。
;;
;; 示例
;; ----
;; (vector-set! #(1 2 3) 1 9) => 修改后对应位置为9
;;
;; 错误处理
;; ----
;; out-of-range 当k超出向量边界时
;; wrong-type-arg 当vec不是向量，或k不是整数时

(let ((v #(1 2 3)))
  (vector-set! v 1 42)
  (check v => #(1 42 3))
) ;let

(let ((v #(a b c d)))
  (vector-set! v 0 'x)
  (vector-set! v 3 'y)
  (check v => #(x b c y))
) ;let

(let ((v #(42)))
  (vector-set! v 0 100)
  (check v => #(100))
) ;let

(let ((v #(1 2 3 4 5)))
  (vector-set! v 0 "string")
  (vector-set! v 1 3.14)
  (vector-set! v 2 'symbol)
  (vector-set! v 3 #\c)
  (vector-set! v 4 #t)
  (check v => #("string" 3.14 symbol #\c #t))
) ;let

(let ((v #(1 2 3)))
  (check-catch 'out-of-range (vector-set! v -1 42))
  (check-catch 'out-of-range (vector-set! v 3 42))
) ;let

(check-report)
