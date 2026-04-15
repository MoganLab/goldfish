(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector->generator
;; 将 flexvector 转换为生成器函数。时间复杂度 O(1) 创建，O(1) 每次调用。
;;
;; 语法
;; ----
;; (flexvector->generator fv)
;;
;; 参数
;; ----
;; fv : flexvector
;;   源向量。
;;
;; 返回值
;; -----
;; 返回一个生成器函数，每次调用返回下一个元素，
;; 遍历完成后返回 eof-object。
;;
;; 另见
;; ----
;; generator->flexvector - 从生成器构造


;; 基本转换
(let ((gen (flexvector->generator (flexvector 'a 'b 'c)
           ) ;flexvector->generator
      ) ;gen
     ) ;
  (check (gen) => 'a)
  (check (gen) => 'b)
  (check (gen) => 'c)
  (check (eof-object? (gen)) => #t)
  ;; 继续调用仍返回 eof
  (check (eof-object? (gen)) => #t)
) ;let


;; 空向量
(let ((gen (flexvector->generator (flexvector))
      ) ;gen
     ) ;
  (check (eof-object? (gen)) => #t)
) ;let


;; 单元素
(let ((gen (flexvector->generator (flexvector 'only)
           ) ;flexvector->generator
      ) ;gen
     ) ;
  (check (gen) => 'only)
  (check (eof-object? (gen)) => #t)
) ;let


;; 修改原向量后生成器行为
(let ((fv (flexvector 1 2 3)) (gen #f))
  (set! gen (flexvector->generator fv))
  (check (gen) => 1)
  (flexvector-set! fv 1 999)
  (check (gen) => 999)
  (check (gen) => 3)
) ;let


;; 用于遍历
(let ((fv (flexvector 10 20 30))
      (sum 0)
      (gen #f)
     ) ;
  (set! gen (flexvector->generator fv))
  (let loop
    ((val (gen)))
    (if (eof-object? val)
      sum
      (begin
        (set! sum (+ sum val))
        (loop (gen))
      ) ;begin
    ) ;if
  ) ;let
  (check sum => 60)
) ;let


(check-report)
