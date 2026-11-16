;; random
;; 验证 s7 内置 random、random-state、random-state? 与 random-state->list 的基本行为、边界条件与优化器路径。
;;
;; 语法
;; ----
;; (random num)
;; (random num state)
;; (random-state)
;; (random-state seed)
;; (random-state seed carry)
;; (random-state? obj)
;; (random-state->list)
;; (random-state->list state)
;;
;; 参数
;; ----
;; num : number
;; 生成随机数的上界（整数、实数、有理数或复数）。
;;
;; state : random-state
;; 可选的随机数状态对象。
;;
;; 返回值
;; ----
;; random 返回与 num 同类型的随机数；random-state 返回状态对象；random-state? 返回布尔值；random-state->list 返回包含 seed 与 carry 的列表。

(import (liii check))

(check-set-mode! 'report-failed)

;; random-state 构造与基本谓词
(let ((s1 (random-state 1234)) (s2 (random-state 1234 5678)) (def (random-state)))
  (check-true (random-state? s1))
  (check-true (random-state? s2))
  (check-true (random-state? def))
  (check-false (random-state? 1234))
  (check-false (random-state? '(1234 5678)))
  (check-false (random-state? #f))
  (check (random-state->list s1) => '(1234 1675393560))
  (check (random-state->list s2) => '(1234 5678))
  (check-true (list? (random-state->list)))
) ;let

;; random-state 错误处理：负数越界与非法类型
(check-catch 'out-of-range (random-state -1))
(check-catch 'out-of-range (random-state 1 -1))
(check-catch 'type-error (random-state "123"))
(check-catch 'type-error (random-state 1 "456"))
(check-catch 'type-error (random-state->list 123))

;; equal? 与 copy 语义
(let* ((s1 (random-state 42 99))
       (s2 (random-state 42 99))
       (s3 (random-state 42 100))
       (s-copy (copy s1))
      ) ;
  (check-true (equal? s1 s2))
  (check-true (equal? s1 s-copy))
  (check-false (eq? s1 s-copy))
  (check-false (equal? s1 s3))
  (check-false (equal? s1 42))
  ;; 推进 s1 后，s-copy 保持原状态
  (let ((v1 (random 1000 s1)))
    (check-false (equal? s1 s-copy))
    (check (random 1000 s-copy) => v1)
    (check-true (equal? s1 s-copy))
  ) ;let
) ;let*

;; random 整数、实数、有理数、复数及 0 边界
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

  ;; 有理数：普通有理数、中等小有理数、极小有理数 (< 1e-10)
  (let ((r1 (random 3/4 s))
        (r2 (random 1/10000000 s))
        (r3 (random 1/1000000000000 s))
        (r4 (random 1/100000000000 s))
        (r5 (random -1/100000000000 s))
       ) ;
    (check-true (rational? r1))
    (check-true (and (>= r1 0) (<= r1 3/4)))
    (check-true (rational? r2))
    (check-true (and (>= r2 0) (<= r2 1/10000000)))
    (check-true (rational? r3))
    (check-true (and (>= r3 0) (<= r3 1/1000000000000)))
    (check-true (rational? r4))
    (check-true (and (>= r4 0) (<= r4 1/100000000000)))
    (check-true (rational? r5))
    (check-true (and (<= r5 0) (>= r5 -1/100000000000)))
  ) ;let

  ;; 复数
  (let ((c (random 3.0+4.0i s)))
    (check-true (complex? c))
    (check-true (and (>= (real-part c) 0.0) (< (real-part c) 3.0)))
    (check-true (and (>= (imag-part c) 0.0) (< (imag-part c) 4.0)))
  ) ;let
) ;let

;; random 非法参数报错
(check-catch 'type-error (random "bad"))
(check-catch 'type-error (random 10 "not-a-state"))

;; 自定义 random-state 可复现性
(let* ((s1 (random-state 777 888)) (s2 (apply random-state (random-state->list s1))))
  (check-true (equal? s1 s2))
  (let loop
    ((i 0))
    (when (< i 20)
      (check (random 10000 s1) => (random 10000 s2))
      (check (random 1.0 s1) => (random 1.0 s2))
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;let*

;; 优化器特化路径：(+ x (random y))、(- (random x) y)、循环中的 (random n)
(let ((saved (copy (*s7* 'default-random-state))))
  (let ((res-i
          (let loop
            ((i 0) (acc 0))
            (if (= i 20)
              acc
              (let ((v (+ 10 (random 5))))
                (check-true (and (integer? v) (>= v 10) (< v 15)))
                (loop (+ i 1) (+ acc v))
              ) ;let
            ) ;if
          ) ;let
        ) ;res-i
        (res-f
          (let loop
            ((i 0) (acc 0.0))
            (if (= i 20)
              acc
              (let ((v (- (random 2.0) 1.0)))
                (check-true (and (real? v) (>= v -1.0) (< v 1.0)))
                (loop (+ i 1) (+ acc v))
              ) ;let
            ) ;if
          ) ;let
        ) ;res-f
       ) ;
    (check-true (integer? res-i))
    (check-true (real? res-f))
  ) ;let
  (set! (*s7* 'default-random-state) saved)
) ;let

;; object->let
(let* ((s (random-state 314 159)) (lt (object->let s)))
  (check-true (let? lt))
  (check (lt 'value) => s)
  (check (lt 'type) => 'random-state?)
  (check (lt 'seed) => 314)
  (check (lt 'carry) => 159)
) ;let*

(check-report)
