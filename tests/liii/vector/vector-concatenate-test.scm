(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-concatenate
;; 连接一个列表中的多个向量并返回新向量。
;;
;; 语法
;; ----
;; (vector-concatenate list-of-vectors)
;;
;; 参数
;; ----
;; list-of-vectors : list? - 一个包含向量的列表。
;;
;; 返回值
;; ----
;; vector - 一个新的向量，包含所有输入向量的元素。
;;
;; 注意
;; ----
;; 空列表返回空向量；列表中的空向量会被正确处理。


(check (vector-concatenate '()) => #())
(check (vector-concatenate '(#(1 2 3))) => #(1 2 3))
(check (vector-concatenate '(#(1 2) #(3 4))) => #(1 2 3 4))
(check (vector-concatenate '(#(a b) #(c d) #(e f))) => #(a b c d e f))
(check (vector-concatenate '(#())) => #())
(check (vector-concatenate '(#() #())) => #())
(check (vector-concatenate '(#() #(1) #())) => #(1))
(check (vector-concatenate '(#(42))) => #(42))
(check (vector-concatenate '(#(1) #(2) #(3))) => #(1 2 3))
(check (vector-concatenate '(#(1 2.5) #("hello" 'symbol) #(#\c #t #f)))
  =>
  #(1 2.5 "hello" 'symbol #\c #t #f)
) ;check
(check (vector-concatenate '(#((1 2)) #((3 4)))) => #((1 2) (3 4)))


(let ((original #(a b c)))
  (let ((result (vector-concatenate (list original))))
    (check-true (vector? result))
    (check-false (eq? original result))
    (check (vector-length result) => (vector-length original))
  ) ;let
) ;let


(let ((v1 #(1 2 3)))
  (let ((v2 #(4 5 6)))
    (let ((result (vector-concatenate (list v1 v2))))
      (vector-set! result 0 99)
      (check v1 => #(1 2 3))
      (check v2 => #(4 5 6))
      (check result => #(99 2 3 4 5 6))
    ) ;let
  ) ;let
) ;let


(check-report)
