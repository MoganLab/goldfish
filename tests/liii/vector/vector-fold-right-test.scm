(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-fold-right
;; 从右到左折叠向量。
;;
;; 语法
;; ----
;; (vector-fold-right proc knil vec)
;;
;; 参数
;; ----
;; proc : procedure?
;; 折叠函数，接收当前元素和累加器。
;;
;; knil : any?
;; 初始累加器值。
;;
;; vec : vector?
;; 要遍历的向量。
;;
;; 返回值
;; ----
;; any?
;; 最终累加结果。
;;
;; 注意
;; ----
;; 与vector-fold不同，遍历顺序是从右到左。
;;
;; 示例
;; ----
;; (vector-fold-right + 0 #(1 2 3 4)) => 10
;; (vector-fold-right string-append "" #("a" "b")) => "ab"
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是向量，或proc不是过程时


(check (vector-fold-right + 0 #(1 2 3 4))
  =>
  10
) ;check
(check (vector-fold-right * 1 #(1 2 3 4))
  =>
  24
) ;check
(check (vector-fold-right (lambda (x acc) (cons x acc))
         '()
         #(1 2 3)
       ) ;vector-fold-right
  =>
  '(1 2 3)
) ;check
(check (vector-fold-right (lambda (x acc)
                            (+ acc (if (even? x) 1 0))
                          ) ;lambda
         0
         #(1 2 3 4)
       ) ;vector-fold-right
  =>
  2
) ;check
(check (vector-fold-right + 0 #()) => 0)
(check (vector-fold-right * 1 #()) => 1)
(check (vector-fold-right + 0 #(5))
  =>
  5
) ;check
(check (vector-fold-right * 1 #(5))
  =>
  5
) ;check
(check (vector-fold-right string-append
         ""
         #("a" "b" "c")
       ) ;vector-fold-right
  =>
  "abc"
) ;check
(check (vector-fold-right (lambda (x acc) (and acc x))
         #t
         #(#t #t #f)
       ) ;vector-fold-right
  =>
  #f
) ;check


(check-report)
