;; random
;; 生成指定上界与类型的随机数（支持整数、实数、有理数和复数）。
;;
;; 语法
;; ----
;; (random num)
;; (random num state)
;;
;; 参数
;; ----
;; num : number
;; 生成随机数的上界，可为整数、实数、有理数或复数。
;;
;; state : random-state
;; 可选的随机数状态对象，默认使用系统默认随机状态。
;;
;; 返回值
;; ----
;; number
;; 返回与 num 同类型的随机数；当 num 为 0 或 0.0 时返回 0 或 0.0。
;;
;; 错误处理
;; ----
;; type-error 当 num 不是数值或 state 不是 random-state 时抛出。

(import (scheme base) (liii random) (liii check))

(check-set-mode! 'report-failed)

;; 默认状态下的基本调用
(let ((v (random 100)))
  (check-true (integer? v))
  (check-true (and (>= v 0) (< v 100)))
) ;let

;; 指定 random-state 的整数、实数、有理数、复数及 0 边界
(let ((s (random-state 2025 13579)))
  ;; 0 边界
  (check (random 0 s) => 0)
  (check (random 0.0 s) => 0.0)

  ;; 整数范围 [0, n)
  (let loop
    ((i 0))
    (when (< i 50)
      (let ((v (random 100 s)))
        (check-true (integer? v))
        (check-true (and (>= v 0) (< v 100)))
      ) ;let
      (loop (+ i 1))
    ) ;when
  ) ;let

  ;; 实数范围 [0.0, x)
  (let loop
    ((i 0))
    (when (< i 50)
      (let ((v (random 10.0 s)))
        (check-true (real? v))
        (check-true (and (>= v 0.0) (< v 10.0)))
      ) ;let
      (loop (+ i 1))
    ) ;when
  ) ;let

  ;; 有理数
  (let ((r1 (random 3/4 s)) (r2 (random 1/10000000 s)) (r3 (random -1/100000000000 s)))
    (check-true (rational? r1))
    (check-true (and (>= r1 0) (<= r1 3/4)))
    (check-true (rational? r2))
    (check-true (and (>= r2 0) (<= r2 1/10000000)))
    (check-true (rational? r3))
    (check-true (and (<= r3 0) (>= r3 -1/100000000000)))
  ) ;let

  ;; 复数
  (let ((c (random 3.0+4.0i s)))
    (check-true (complex? c))
    (check-true (and (>= (real-part c) 0.0) (< (real-part c) 3.0)))
    (check-true (and (>= (imag-part c) 0.0) (< (imag-part c) 4.0)))
  ) ;let
) ;let

;; 相同种子状态生成相同的随机数序列
(let ((s1 (random-state 777 888)) (s2 (random-state 777 888)))
  (let loop
    ((i 0))
    (when (< i 20)
      (check (random 10000 s1) => (random 10000 s2))
      (check (random 1.0 s1) => (random 1.0 s2))
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;let

;; 非法参数报错
(check-catch 'type-error (random "bad"))
(check-catch 'type-error (random 10 "not-a-state"))

(check-report)
