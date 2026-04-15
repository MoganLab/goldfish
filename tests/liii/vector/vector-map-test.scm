(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-map
;; 对向量中的每个元素应用函数并返回新向量。
;;
;; 语法
;; ----
;; (vector-map proc vec1 vec2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 应用于每组元素的过程。
;;
;; vec1 vec2 ... : vector?
;; 一个或多个参与映射的向量。
;;
;; 返回值
;; ----
;; vector
;; 一个新的结果向量，包含每次proc调用的返回值。
;;
;; 注意
;; ----
;; 多个向量会按对应位置并行映射。
;;
;; 示例
;; ----
;; (vector-map + #(1 2 3) #(4 5 6)) => #(5 7 9)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当proc不是过程，或任一参数不是向量时


(check (vector-map (lambda (x) (* x 2))
         #(1 2 3)
       ) ;vector-map
  =>
  #(2 4 6)
) ;check
(check (vector-map (lambda (x) (string-append x "!"))
         #("a" "b" "c")
       ) ;vector-map
  =>
  #("a!" "b!" "c!")
) ;check
(check (vector-map + #(1 2 3) #(4 5 6))
  =>
  #(5 7 9)
) ;check
(check (vector-map cons #(a b c) #(1 2 3))
  =>
  #((a . 1) (b . 2) (c . 3))
) ;check
(check (vector-map (lambda (x) (* x 2)) #())
  =>
  #()
) ;check
(check (vector-map (lambda (x) (+ x 10)) #(5))
  =>
  #(15)
) ;check
(check-catch 'wrong-type-arg
  (vector-map 'not-a-proc #(1 2 3))
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-map + 'not-a-vector)
) ;check-catch


(check-report)
