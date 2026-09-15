(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; generator-count
;; 统计生成器产出序列中满足谓词条件的元素总数。
;;
;; 语法
;; ----
;; (generator-count pred gen)
;;
;; 参数
;; ----
;; pred : procedure
;; 单参谓词过程。
;;
;; gen : procedure
;; 输入生成器。
;;
;; 返回值
;; ----
;; exact-nonnegative-integer
;; 满足条件的元素个数。
;;
;; 错误处理
;; ----
;; 无

(let ((g (generator 1 2 3 4 5)))
  (check (generator-count even? g) => 2)
) ;let

(let ((g (generator 1 3 5)))
  (check (generator-count even? g) => 0)
) ;let

(let ((g (generator)))
  (check (generator-count even? g) => 0)
) ;let

;; 统计小于 100 的 Fibonacci 数数量
(let* ((fib-gen
         (make-coroutine-generator
           (lambda (yield)
             (let loop
               ((a 0) (b 1))
               (when (<= a 200)
                 (yield a)
                 (loop b (+ a b))
               ) ;when
             ) ;let
           ) ;lambda
         ) ;make-coroutine-generator
       ) ;fib-gen
      ) ;
  (check (generator-count (lambda (x) (< x 100)) fib-gen) => 12)
) ;let*

;; 闭包生成器自身返回 eof-object（不依赖外部截断函数）
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda ()
             (if (> a 200) (eof-object) (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
           ) ;lambda
         ) ;let
       ) ;fib-gen
      ) ;
  (check (generator-count (lambda (x) (< x 100)) fib-gen) => 12)
) ;let*

;; 无限生成器使用 gtake 截取前 N 项再统计
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda () (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
         ) ;let
       ) ;fib-gen
      ) ;
  (check (generator-count (lambda (x) (< x 100)) (gtake fib-gen 20)) => 12)
) ;let*

;; 无限生成器使用 gtake-while 按条件截断
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda () (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
         ) ;let
       ) ;fib-gen
      ) ;
  (check
    (generator-count (lambda (x) (< x 100))
      (gtake-while (lambda (x) (< x 100)) fib-gen)
    ) ;generator-count
    =>
    12
  ) ;check
) ;let*

(check-report)
