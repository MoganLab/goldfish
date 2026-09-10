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
;; 错误处理
;; ----
;; type-error 当任一参数不是向量时


(check (vector-append) => #())
(check (vector-append #(1 2 3)) => #(1 2 3))
(check (vector-append #(1 2) #(3 4)) => #(1 2 3 4))
(check (vector-append #(a b) #(c d) #(e f)) => #(a b c d e f))
(check (vector-append #()) => #())
(check (vector-append #() #()) => #())
(check (vector-append #() #(1) #()) => #(1))
(check (vector-append #(42)) => #(42))
(check (vector-append #(1) #(2) #(3)) => #(1 2 3))
(check (vector-append #(1 2.5) #("hello" 'symbol) #(#\c #t #f))
  =>
  #(1 2.5 "hello" 'symbol #\c #t #f)
) ;check
(check (vector-append #((1 2)) #((3 4))) => #((1 2) (3 4)))


(let ((original #(a b c)))
  (let ((appended (vector-append original)))
    (check-true (vector? appended))
    (check-false (eq? original appended))
    (check (vector-length appended) => (vector-length original))
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


(check-catch 'type-error (vector-append 'not-a-vector))
(check-catch 'type-error (vector-append #(1 2) 'not-a-vector))
(check-catch 'type-error (vector-append #(1 2) 3 #(4 5)))


;; openlet 方法分派测试：openlet 位于首参 / 非首参 / 中间位置
(check (vector-append (openlet (inlet 'vector-append (lambda (self . rest) (apply vector-append #(0) rest)))
                      ) ;openlet
         #(1 2)
       ) ;vector-append
  =>
  #(0 1 2)
) ;check

(check (vector-append #(1 2)
         (openlet (inlet 'vector-append (lambda (vec self) (vector-append vec #(99)))))
       ) ;vector-append
  =>
  #(1 2 99)
) ;check

(check (vector-append #(1)
         #(2)
         (openlet (inlet 'vector-append
                    (lambda (vec self . rest) (apply vector-append vec #(99) rest))
                  ) ;inlet
         ) ;openlet
         #(3)
         #(4)
       ) ;vector-append
  =>
  #(1 2 99 3 4)
) ;check

;; GC 压力回归测试：openlet vector-append 方法内触发 GC
(let ((v #(1 2)))
  (do ((i 0 (+ i 1)))
    ((= i 5))
    (let ((o (openlet (inlet 'vector-append (lambda (vec self) (gc) (vector-append vec #(3))))
             ) ;openlet
          ) ;o
         ) ;
      (check (vector-append v o) => #(1 2 3))
    ) ;let
  ) ;do
) ;let


(check-report)
