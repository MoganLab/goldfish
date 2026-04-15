(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-contains?
;; 检查向量中是否包含指定元素。
;;
;; 语法
;; ----
;; (vector-contains? vec elem)
;; (vector-contains? vec elem compare)
;;
;; 参数
;; ----
;; vec : vector?
;; 要查找的向量。
;;
;; elem : any?
;; 要匹配的目标元素。
;;
;; compare : procedure? 可选
;; 自定义比较函数，默认使用equal?。
;;
;; 返回值
;; ----
;; boolean
;; 找到匹配元素时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; compare会按(compare 元素 elem)的方式参与判断。
;;
;; 示例
;; ----
;; (vector-contains? #(1 2 3) 2) => #t
;; (vector-contains? #(1 2 3) 4) => #f
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量，或compare不是过程时


(check-true (vector-contains? #(1 2 3) 2)
) ;check-true
(check-false (vector-contains? #(1 2 3) 4)
) ;check-false
(check-false (vector-contains? #() 1))
(check-true (vector-contains? #(a b c) 'b)
) ;check-true
(check-true (vector-contains? #("hello" "world")
              "hello"
            ) ;vector-contains?
) ;check-true
(check-true (vector-contains? #(#\a #\b #\c) #\b)
) ;check-true
(check-true (vector-contains? #(1 2 3) 2 =)
) ;check-true
(check-false (vector-contains? #(1 2 3) 4 =)
) ;check-false
(check-true (vector-contains? #((1 2) (3 4))
              '(1 2)
              equal?
            ) ;vector-contains?
) ;check-true
(check-true (vector-contains? #(42) 42))
(check-false (vector-contains? #(42) 0))
(check-true (vector-contains? #(1 2 3) 1)
) ;check-true
(check-true (vector-contains? #(1 2 3) 3)
) ;check-true
(check-catch 'type-error
  (vector-contains? 'not-a-vector 1)
) ;check-catch


(check-report)
