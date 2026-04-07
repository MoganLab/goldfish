(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; random-source-state-ref
;; 获取随机源的当前状态。
;;
;; 语法
;; ----
;; (random-source-state-ref s)
;;
;; 参数
;; ----
;; s : random-source
;; 要获取状态的随机源。
;;
;; 返回值
;; ----
;; list
;; 返回形式为 (random-source-state seed carry) 的列表。
;;
;; 示例
;; ----
;; (random-source-state-ref (make-random-source))
;;   => (random-source-state 0 1675393560)
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 s 不是随机源时抛出。

; 基本功能测试
(let ((s (make-random-source)))
  (let ((state (random-source-state-ref s)))
    (check (pair? state) => #t)
    (check (eq? (car state) 'random-source-state) => #t)
    (check (= (length state) 3) => #t)
  ) ;let
) ;let

; 状态在生成随机数后变化
(let ((s (make-random-source)))
  (let ((state1 (random-source-state-ref s)))
    (let ((rand-int (random-source-make-integers s)))
      (rand-int 100)
      (let ((state2 (random-source-state-ref s)))
        (check (not (equal? state1 state2)) => #t)
      ) ;let
    ) ;let
  ) ;let
) ;let

; 错误处理
(check-catch 'wrong-type-arg (random-source-state-ref 'not-a-source))
(check-catch 'wrong-type-arg (random-source-state-ref 123))
(check-catch 'wrong-type-arg (random-source-state-ref "string"))

(check-report)
