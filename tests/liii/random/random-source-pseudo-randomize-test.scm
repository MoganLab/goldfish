(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; random-source-pseudo-randomize!
;; 使用索引 (i, j) 确定性地设置随机源状态。
;;
;; 语法
;; ----
;; (random-source-pseudo-randomize! s i j)
;;
;; 参数
;; ----
;; s : random-source
;; 要设置的随机源。
;;
;; i, j : non-negative-exact-integer
;; 索引参数，必须是非负精确整数。
;;
;; 返回值
;; ----
;; undefined
;; 副作用：修改 s 的状态。
;;
;; 示例
;; ----
;; (let ((s (make-random-source)))
;;   (random-source-pseudo-randomize! s 5 10)
;;   s)  ; s 现在处于确定状态
;;
;; 注意
;; ----
;; 相同的索引对总是产生相同的状态。

; 相同索引产生相同状态
(let ((s1 (make-random-source))
      (s2 (make-random-source)))
  (random-source-pseudo-randomize! s1 0 0)
  (random-source-pseudo-randomize! s2 0 0)
  (let ((state1 (random-source-state-ref s1))
        (state2 (random-source-state-ref s2)))
    (check (equal? state1 state2) => #t)
  ) ;let
) ;let

; 不同索引产生不同状态
(let ((s (make-random-source)))
  (random-source-pseudo-randomize! s 0 0)
  (let ((state1 (random-source-state-ref s)))
    (random-source-pseudo-randomize! s 1 2)
    (let ((state2 (random-source-state-ref s)))
      (check (not (equal? state1 state2)) => #t)
    ) ;let
  ) ;let
) ;let

; 错误处理
(check-catch 'wrong-type-arg (random-source-pseudo-randomize! 'not-a-source 0 0))

(let ((s (make-random-source)))
  (check-catch 'wrong-type-arg (random-source-pseudo-randomize! s -1 0))
  (check-catch 'wrong-type-arg (random-source-pseudo-randomize! s 0 -1))
  (check-catch 'wrong-type-arg (random-source-pseudo-randomize! s 3.14 0))
  (check-catch 'wrong-type-arg (random-source-pseudo-randomize! s 0 3.14))
) ;let

(check-report)
