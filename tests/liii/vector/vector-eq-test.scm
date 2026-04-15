(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector=
;; 使用给定比较函数比较多个向量。
;;
;; 语法
;; ----
;; (vector= elt=? vec ...)
;;
;; 参数
;; ----
;; elt=? : procedure?
;; 用于比较元素是否相等的函数。
;;
;; vec : vector?
;; 一个或多个待比较的向量。
;;
;; 返回值
;; ----
;; boolean
;; 当所有向量长度一致且对应元素都满足elt=?时返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 允许只传比较函数，此时结果为#t。
;;
;; 示例
;; ----
;; (vector= = #(1 2) #(1 2)) => #t
;; (vector= eq? #(a b) #(a c)) => #f
;;
;; 错误处理
;; ----
;; type-error 当elt=?不是过程时


(check-true (vector= eq?))
(check-true (vector= eq? '#(a)))
(check-true (vector= eq? '#(a b c d) '#(a b c d))
) ;check-true
(check-false (vector= eq? '#(a b c d) '#(a b d c))
) ;check-false
(check-false (vector= = '#(1 2 3 4 5) '#(1 2 3 4))
) ;check-false
(check-true (vector= = '#(1 2 3 4) '#(1 2 3 4))
) ;check-true
(check-true (vector= equal?
              '#(1 2 3)
              '#(1 2 3)
              '#(1 2 3)
            ) ;vector=
) ;check-true
(check-false (vector= equal?
               '#(1 2 3)
               '#(1 2 3)
               '#(1 2 3 4)
             ) ;vector=
) ;check-false
(check-catch 'type-error
  (vector= 1
    (vector (vector 'a))
    (vector (vector 'a))
  ) ;vector=
) ;check-catch
(check-true (vector= equal?
              (vector (vector 'a))
              (vector (vector 'a))
            ) ;vector=
) ;check-true
(check-false (vector= eq?
               (vector (vector 'a))
               (vector (vector 'a))
             ) ;vector=
) ;check-false


(check-report)
