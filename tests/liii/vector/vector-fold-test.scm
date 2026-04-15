(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-fold
;; 从左到右折叠向量。
;;
;; 语法
;; ----
;; (vector-fold proc knil vec)
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
;; 本实现中的proc参数顺序为(当前元素, 累加器)。
;;
;; 示例
;; ----
;; (vector-fold + 0 #(1 2 3 4)) => 10
;; (vector-fold string-append "" #("a" "b")) => "ba"
;;
;; 错误处理
;; ----
;; wrong-type-arg 当vec不是向量，或proc不是过程时


(check (vector-fold + 0 #(1 2 3 4))
  =>
  10
) ;check
(check (vector-fold * 1 #(1 2 3 4))
  =>
  24
) ;check
(check (vector-fold (lambda (x acc) (cons x acc))
         '()
         #(1 2 3)
       ) ;vector-fold
  =>
  '(3 2 1)
) ;check
(check (vector-fold (lambda (x acc)
                      (+ acc (if (even? x) 1 0))
                    ) ;lambda
         0
         #(1 2 3 4)
       ) ;vector-fold
  =>
  2
) ;check
(check (vector-fold + 0 #()) => 0)
(check (vector-fold * 1 #()) => 1)
(check (vector-fold + 0 #(5)) => 5)
(check (vector-fold * 1 #(5)) => 5)
(check (vector-fold string-append
         ""
         #("a" "b" "c")
       ) ;vector-fold
  =>
  "cba"
) ;check
(check (vector-fold (lambda (x acc) (and acc x))
         #t
         #(#t #t #f)
       ) ;vector-fold
  =>
  #f
) ;check


(check-report)
