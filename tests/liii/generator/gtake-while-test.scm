(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gtake-while
;; 持续从生成器产出元素，直至首个不满足谓词条件的元素出现时终止。
;;
;; 语法
;; ----
;; (gtake-while pred gen)
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
;; procedure
;; 新生成器过程。依次产出元素，遇到首个使 pred 为 #f 的元素后永久终止（返回 eof-object）。
;;
;; 错误处理
;; ----
;; 无

(let ((g (gtake-while odd? (generator 1 3 4 5))))
  (check (generator->list g) => '(1 3))
) ;let

(let ((g (gtake-while even? (generator 1 2 3))))
  (check (generator->list g) => '())
) ;let

;; 获取 gtake-while 之后的生成器长度：
;; 方式 1：转换为列表后使用 length 获取长度（保留元素）
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda () (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
         ) ;let
       ) ;fib-gen
       (g (gtake-while (lambda (x) (< x 100)) fib-gen))
       (lst (generator->list g))
      ) ;
  (check (length lst) => 12)
  (check lst => '(0 1 1 2 3 5 8 13 21 34 55 89))
) ;let*

;; 方式 2：使用 generator-count 统计所有元素个数（O(1) 内存消耗流）
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda () (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
         ) ;let
       ) ;fib-gen
       (g (gtake-while (lambda (x) (< x 100)) fib-gen))
      ) ;
  (check (generator-count (lambda (x) #t) g) => 12)
) ;let*

;; 方式 3：使用 generator-fold 折叠累计计数
(let* ((fib-gen
         (let ((a 0) (b 1))
           (lambda () (let ((curr a)) (set! a b) (set! b (+ curr b)) curr))
         ) ;let
       ) ;fib-gen
       (g (gtake-while (lambda (x) (< x 100)) fib-gen))
      ) ;
  (check (generator-fold (lambda (item count) (+ count 1)) 0 g) => 12)
) ;let*

(check-report)
