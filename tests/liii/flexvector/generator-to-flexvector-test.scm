(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; generator->flexvector
;; 从生成器读取元素构造 flexvector，直到遇到 eof-object。时间复杂度 O(n)。
;;
;; 语法
;; ----
;; (generator->flexvector gen)
;;
;; 参数
;; ----
;; gen : procedure
;;   生成器函数，每次调用返回一个元素或 eof-object。
;;
;; 返回值
;; -----
;; 返回新的 flexvector，包含生成器产生的所有元素。
;;
;; 示例
;; ----
;; ;; 使用生成器构造
;; (define (make-counter n)
;;   (lambda ()
;;     (if (< n 0)
;;       (eof-object)
;;       (let ((v n))
;;         (set! n (- n 1))
;;         v))))
;; (generator->flexvector (make-counter 5))
;; => #<flexvector: 5 4 3 2 1 0>
;;
;; 另见
;; ----
;; flexvector->generator - 转换为生成器
;; flexvector-unfold - 类似功能


;; 基本转换
(let ((genlist '(a b c)))
  (define (mock-generator)
    (if (pair? genlist)
      (let ((value (car genlist)))
        (set! genlist (cdr genlist))
        value
      ) ;let
      (eof-object)
    ) ;if
  ) ;define
  (check (flexvector->list (generator->flexvector mock-generator)
         ) ;flexvector->list
    =>
    '(a b c)
  ) ;check
) ;let


;; 空生成器
(let ((gen (lambda () (eof-object))))
  (check (flexvector->vector (generator->flexvector gen)
         ) ;flexvector->vector
    =>
    #()
  ) ;check
) ;let


;; 单元素
(let ((gen (let ((called #f))
             (lambda ()
               (if called
                 (eof-object)
                 (begin
                   (set! called #t)
                   'only
                 ) ;begin
               ) ;if
             ) ;lambda
           ) ;let
      ) ;gen
     ) ;
  (check (flexvector->list (generator->flexvector gen)
         ) ;flexvector->list
    =>
    '(only)
  ) ;check
) ;let


;; 计数器生成器
(let ((counter (let ((n 0))
                 (lambda ()
                   (if (< n 5)
                     (begin
                       (set! n (+ n 1))
                       n
                     ) ;begin
                     (eof-object)
                   ) ;if
                 ) ;lambda
               ) ;let
      ) ;counter
     ) ;
  (check (flexvector->list (generator->flexvector counter)
         ) ;flexvector->list
    =>
    '(1 2 3 4 5)
  ) ;check
) ;let


;; 递减生成器
(let ((gen (let ((n 10))
             (lambda ()
               (if (< n 0)
                 (eof-object)
                 (begin
                   (set! n (- n 2))
                   (+ n 2)
                 ) ;begin
               ) ;if
             ) ;lambda
           ) ;let
      ) ;gen
     ) ;
  (check (flexvector->list (generator->flexvector gen)
         ) ;flexvector->list
    =>
    '(10 8 6 4 2 0)
  ) ;check
) ;let


(check-report)
