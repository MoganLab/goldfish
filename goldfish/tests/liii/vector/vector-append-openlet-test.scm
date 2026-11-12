;; vector-append-openlet
;; vector-append 的 openlet 方法分派测试：openlet 覆盖同名方法后，内建的
;; vector-append 会转调该方法，方法可位于首参 / 非首参 / 中间位置。
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
;; vector?
;; 所有输入向量按顺序连接后的新向量。

(import (liii check) (liii vector))


(check-set-mode! 'report-failed)

;; openlet 方法分派测试：openlet 位于首参 / 非首参 / 中间位置
(check
  (vector-append
    (openlet (inlet 'vector-append (lambda (self . rest) (apply vector-append #(0) rest)))
    ) ;openlet
    #(1 2)
  ) ;vector-append
  =>
  #(0 1 2)
) ;check

(check
  (vector-append #(1 2)
    (openlet (inlet 'vector-append (lambda (vec self) (vector-append vec #(99)))))
  ) ;vector-append
  =>
  #(1 2 99)
) ;check

(check
  (vector-append #(1)
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
    (let ((o
            (openlet (inlet 'vector-append (lambda (vec self) (gc) (vector-append vec #(3))))
            ) ;openlet
          ) ;o
         ) ;
      (check (vector-append v o) => #(1 2 3))
    ) ;let
  ) ;do
) ;let


(check-report)
