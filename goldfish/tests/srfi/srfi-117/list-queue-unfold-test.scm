;; list-queue-unfold
;; 由种子逐步展开，向队列头部依次添加元素生成队列。
;;
;; 语法
;; ----
;; (list-queue-unfold stop? mapper successor seed . rest)
;;
;; 参数
;; ----
;; stop? : procedure?
;; 谓词，作用于当前种子，返回真时停止展开。
;;
;; mapper : procedure?
;; 单参过程，将种子映射为队列元素。
;;
;; successor : procedure?
;; 单参过程，由当前种子求出下一个种子。
;;
;; seed : any
;; 初始种子。
;;
;; rest : (queue)
;; 可选参数。queue 为已存在的队列，展开的元素被添加到其前端。
;;
;; 返回值
;; ------
;; list-queue?
;; 新队列。展开过程先递归到终点，再从靠近种子的一端依次 add-front!，
;; 因此距离种子越近的元素越靠前。
;;
;; 注意
;; ----
;; 该函数会原地修改作为第 5 个参数传入的队列。

(import (liii check) (srfi srfi-117))


(check-set-mode! 'report-failed)


;; list-queue-unfold 基本测试
(let ((q (list-queue-unfold (lambda (x) (> x 3))
           (lambda (x) (* x 2))
           (lambda (x) (+ x 1))
           0
         ) ;list-queue-unfold
      ) ;q
     ) ;
  ;; unfold: 递归结束后从头部添加，所以第一个元素最后被添加，保持正向顺序
  (check (list-queue-list q) => '(0 2 4 6))
) ;let


;; 带初始队列的 unfold
(let ((q0 (list-queue 8)))
  (let ((q (list-queue-unfold (lambda (x) (> x 3))
             (lambda (x) (* x 2))
             (lambda (x) (+ x 1))
             0
             q0
           ) ;list-queue-unfold
        ) ;q
       ) ;
    (check (list-queue-list q) => '(0 2 4 6 8))
  ) ;let
) ;let


;; 立即终止的情况
(let ((q (list-queue-unfold (lambda (x) (> x 0)) (lambda (x) x) (lambda (x) (+ x 1)) 1)
      ) ;q
     ) ;
  (check (list-queue-empty? q) => #t)
) ;let


(check-report)
