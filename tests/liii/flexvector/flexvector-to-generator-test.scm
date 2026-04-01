(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector->generator
;; 将可变长向量转换为生成器。
;;
;; 语法
;; ----
;; (flexvector->generator fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 源向量。
;;
;; 返回值
;; ----
;; function
;; 生成器函数，每次调用返回下一个元素。
;;
;; 描述
;; ----
;; 创建一个生成器，按顺序产生向量的元素，结束后返回 eof 对象。

(let ((gen (flexvector->generator (flexvector 'a 'b 'c))))
  (check (gen) => 'a)
  (check (gen) => 'b)
  (check (gen) => 'c)
  (check (eof-object? (gen)) => #t)
) ;let

(let ((gen (flexvector->generator (flexvector))))
  (check (eof-object? (gen)) => #t)
) ;let

(check-report)
