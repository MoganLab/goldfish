(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; random-source-make-reals
;; 从随机源创建实数生成器。
;;
;; 语法
;; ----
;; (random-source-make-reals s)
;; (random-source-make-reals s unit)
;;
;; 参数
;; ----
;; s : random-source
;; 用于生成随机数的随机源。
;;
;; unit : real (可选)
;; 精度单位，必须是 (0, 1) 范围内的实数。
;;
;; 返回值
;; ----
;; procedure
;; 返回一个无参数过程，生成 (0, 1) 的随机实数。
;;
;; 示例
;; ----
;; (let* ((s (make-random-source))
;;        (rand-real (random-source-make-reals s)))
;;   (rand-real))  => (0, 1) 之间的随机实数
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 s 不是随机源或 unit 无效时抛出。

; 创建实数生成器
(let* ((s (make-random-source))
       (rand-real (random-source-make-reals s)))
  (check (procedure? rand-real) => #t)
) ;let*

; 生成器工作正常
(let* ((s (make-random-source))
       (rand-real (random-source-make-reals s)))
  (let ((r (rand-real)))
    (check (real? r) => #t)
    (check (> r 0) => #t)
    (check (< r 1) => #t)
  ) ;let
) ;let*

; 带 unit 参数
(let* ((s (make-random-source))
       (rand-real (random-source-make-reals s 0.001)))
  (let ((r (rand-real)))
    (check (real? r) => #t)
    (check (> r 0) => #t)
    (check (< r 1) => #t)
  ) ;let
) ;let*

; 错误处理
(check-catch 'wrong-type-arg (random-source-make-reals 'not-a-source))

(let ((s (make-random-source)))
  (check-catch 'wrong-type-arg (random-source-make-reals s 0))
  (check-catch 'wrong-type-arg (random-source-make-reals s 1))
  (check-catch 'wrong-type-arg (random-source-make-reals s -0.5))
) ;let

(check-report)
