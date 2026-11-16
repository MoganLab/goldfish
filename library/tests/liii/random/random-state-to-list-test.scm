;; random-state->list
;; 将随机数状态对象转换为包含 seed 和 carry 的列表。
;;
;; 语法
;; ----
;; (random-state->list)
;; (random-state->list state)
;;
;; 参数
;; ----
;; state : random-state
;; 可选的随机数状态对象，默认使用系统默认随机状态。
;;
;; 返回值
;; ----
;; list
;; 返回包含两个整数 (seed carry) 的列表。
;;
;; 错误处理
;; ----
;; type-error 当 state 不是 random-state 时抛出。

(import (scheme base) (liii random) (liii check))

(check-set-mode! 'report-failed)

;; 默认随机状态转换为列表
(let ((lst (random-state->list)))
  (check-true (list? lst))
  (check (length lst) => 2)
  (check-true (integer? (car lst)))
  (check-true (integer? (cadr lst)))
) ;let

;; 指定 random-state 转换为列表及往返重建
(let* ((s1 (random-state 777 888))
       (lst (random-state->list s1))
       (s2 (apply random-state lst))
      ) ;
  (check lst => '(777 888))
  (check-true (equal? s1 s2))
  (check (random 1000 s1) => (random 1000 s2))
) ;let*

;; 单参 random-state 的状态列表
(check (random-state->list (random-state 1234)) => '(1234 1675393560))

;; 非法参数报错
(check-catch 'type-error (random-state->list 123))
(check-catch 'type-error (random-state->list "bad"))

(check-report)
