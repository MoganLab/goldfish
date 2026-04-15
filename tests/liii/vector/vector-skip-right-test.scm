(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-skip-right
;; 从右到左跳过满足谓词的元素，返回首个不满足谓词的索引。
;;
;; 语法
;; ----
;; (vector-skip-right pred vec)
;;
;; 参数
;; ----
;; pred : procedure?
;; 用于判断是否继续跳过当前元素的谓词。
;;
;; vec : vector?
;; 要搜索的向量。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回从右侧看第一个不满足pred的元素索引；若全部满足则返回#f。
;;
;; 注意
;; ----
;; 搜索方向为从右到左。
;;
;; 示例
;; ----
;; (vector-skip-right even? #(1 2 3 4)) => 2
;; (vector-skip-right odd? #(1 3 5)) => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当pred不是过程，或vec不是向量时


(check (vector-skip-right even? #(1 2 3 4))
  =>
  2
) ;check
(check (vector-skip-right odd? #(1 3 5 7))
  =>
  #f
) ;check
(check (vector-skip-right (lambda (x) (< x 5))
         #(1 2 3 4 5)
       ) ;vector-skip-right
  =>
  4
) ;check
(check (vector-skip-right (lambda (x) (char=? x #\a))
         #(#\a #\a #\b #\c)
       ) ;vector-skip-right
  =>
  3
) ;check
(check (vector-skip-right even? #())
  =>
  #f
) ;check
(check (vector-skip-right even? #(1))
  =>
  0
) ;check
(check (vector-skip-right odd? #(2))
  =>
  0
) ;check
(check (vector-skip-right (lambda (x) (string=? x "a"))
         #("a" "a" "b" "c")
       ) ;vector-skip-right
  =>
  3
) ;check
(check (vector-skip-right (lambda (x) (eq? x #t))
         #(#t #t #f #t)
       ) ;vector-skip-right
  =>
  2
) ;check
(check (vector-skip-right (lambda (x) (> x 0))
         #(1 2 3 4)
       ) ;vector-skip-right
  =>
  #f
) ;check
(check (vector-skip-right (lambda (x) (char-alphabetic? x))
         #(#\a #\b #\c)
       ) ;vector-skip-right
  =>
  #f
) ;check


(check-report)
