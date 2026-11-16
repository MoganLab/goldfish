;; random-state
;; 创建随机数生成器状态对象。
;;
;; 语法
;; ----
;; (random-state)
;; (random-state seed)
;; (random-state seed carry)
;;
;; 参数
;; ----
;; seed : integer
;; 可选的非负整数种子。
;;
;; carry : integer
;; 可选的非负整数进位值。
;;
;; 返回值
;; ----
;; random-state
;; 返回新创建的随机数状态对象。
;;
;; 错误处理
;; ----
;; out-of-range 当 seed 或 carry 为负数时抛出。
;; type-error 当 seed 或 carry 不是整数时抛出。

(import (scheme base) (liii random) (liii check))

(check-set-mode! 'report-failed)

;; 无参、单参、双参构造
(let ((def (random-state))
      (s1 (random-state 1234))
      (s2 (random-state 1234 5678))
     ) ;
  (check-true (random-state? def))
  (check-true (random-state? s1))
  (check-true (random-state? s2))
  (check (random-state->list s1) => '(1234 1675393560))
  (check (random-state->list s2) => '(1234 5678))
) ;let

;; 0 边界
(let ((s0 (random-state 0 0)))
  (check-true (random-state? s0))
  (check (random-state->list s0) => '(0 0))
) ;let

;; equal? 与独立状态语义
(let ((s1 (random-state 42 99))
      (s2 (random-state 42 99))
      (s3 (random-state 42 100))
     ) ;
  (check-true (equal? s1 s2))
  (check-false (eq? s1 s2))
  (check-false (equal? s1 s3))
  (let ((v1 (random 1000 s1)))
    (check-false (equal? s1 s2))
    (check (random 1000 s2) => v1)
    (check-true (equal? s1 s2))
  ) ;let
) ;let

;; 错误处理：负数越界与非法类型
(check-catch 'out-of-range (random-state -1))
(check-catch 'out-of-range (random-state 1 -1))
(check-catch 'type-error (random-state "123"))
(check-catch 'type-error (random-state 1 "456"))

(check-report)
