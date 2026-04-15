(import (liii check) (liii vector))


(check-set-mode! 'report-failed)


;; vector-append
;; 连接多个向量并返回新向量。
;;
;; 语法
;; ----
;; (vector-append vec ...)
;;
;; 参数
;; ----
;; vec : vector?
;; 零个或多个要连接的向量。
;;
;; 返回值
;; ----
;; vector
;; 一个新的向量，包含所有输入向量的元素。
;;
;; 注意
;; ----
;; 不传参数时返回空向量；只传一个向量时返回其副本。
;;
;; 示例
;; ----
;; (vector-append #(1 2) #(3 4)) => #(1 2 3 4)
;; (vector-append) => #()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当任一参数不是向量时


(check (vector-append) => #())
(check (vector-append #(1 2 3))
  =>
  #(1 2 3)
) ;check
(check (vector-append #(1 2) #(3 4))
  =>
  #(1 2 3 4)
) ;check
(check (vector-append #(a b) #(c d) #(e f))
  =>
  #(a b c d e f)
) ;check
(check (vector-append #()) => #())
(check (vector-append #() #()) => #())
(check (vector-append #() #(1) #())
  =>
  #(1)
) ;check
(check (vector-append #(42)) => #(42))
(check (vector-append #(1) #(2) #(3))
  =>
  #(1 2 3)
) ;check
(check (vector-append #(1 2.5)
         #("hello" (#_quote symbol))
         #(#\c #t #f)
       ) ;vector-append
  =>
  #(1 2.5 "hello" (#_quote symbol) #\c #t #f)
) ;check
(check (vector-append #((1 2)) #((3 4)))
  =>
  #((1 2) (3 4))
) ;check


(let ((original #(a b c)))
  (let ((appended (vector-append original)))
    (check-true (vector? appended))
    (check-false (eq? original appended))
    (check (vector-length appended)
      =>
      (vector-length original)
    ) ;check
  ) ;let
) ;let


(let ((v1 #(1 2 3)))
  (let ((v2 #(4 5 6)))
    (let ((result (vector-append v1 v2)))
      (vector-set! result 0 99)
      (check v1 => #(1 2 3))
      (check v2 => #(4 5 6))
      (check result => #(99 2 3 4 5 6))
    ) ;let
  ) ;let
) ;let


(check-catch 'wrong-type-arg
  (vector-append 'not-a-vector)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-append #(1 2) 'not-a-vector)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-append #(1 2) 3 #(4 5))
) ;check-catch


(check-report)
