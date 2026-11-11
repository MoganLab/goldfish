;; randbelow
;; 在 [0, n) 区间内产生密码学安全且无模偏差的均匀随机整数。
;;
;; 语法
;; ----
;; (randbelow n)
;;
;; 参数
;; ----
;; n : exact-integer?
;; 区间上界（不含），必须是正精确整数。
;;
;; 返回值
;; -----
;; exact-integer?
;; [0, n) 区间内的均匀随机整数。
;;
;; 说明
;; ----
;; 1. 采用拒绝采样（Rejection Sampling）：采样宽度取满足 256^k >= n 的
;;    最小字节数 k，抽到 >= n 的值时丢弃重抽，从而保证 [0, n) 内每个整数
;;    出现的概率严格相等，不会出现直接取模带来的模偏差。
;; 2. 随机性来自操作系统熵源，不可预测。
;;
;; 错误处理
;; ----
;; n 不是精确整数时抛出 type-error；n 小于等于 0 时抛出 value-error。

(import (liii check) (liii secrets))

(check-set-mode! 'report-failed)

;; 结果始终落在 [0, n) 区间内
(check (randbelow 1) => 0)
(check (< (randbelow 2) 2) => #t)
(check (< (randbelow 10) 10) => #t)
(check (< (randbelow 256) 256) => #t)
(check (< (randbelow 1000) 1000) => #t)
(check (>= (randbelow 1000) 0) => #t)

;; 结果必须是精确整数
(check (exact-integer? (randbelow 100)) => #t)
(check (exact? (randbelow 100)) => #t)

;; 下界 0 必须能被取到
(let loop
  ((i 0) (hit-zero #f) (hit-max #f))
  (if (>= i 500)
    (begin
      (check hit-zero => #t)
      (check hit-max => #t)
    ) ;begin
    (let ((v (randbelow 2)))
      (loop (+ i 1) (or hit-zero (= v 0)) (or hit-max (= v 1)))
    ) ;let
  ) ;if
) ;let

;; 采样覆盖整个区间：n = 5 时 0..4 都应出现
(let loop
  ((i 0) (seen '()))
  (if (>= i 500)
    (check (= (length seen) 5) => #t)
    (let ((v (randbelow 5)))
      (loop (+ i 1) (if (member v seen) seen (cons v seen)))
    ) ;let
  ) ;if
) ;let

;; 拒绝采样的正确性：跨越 2^8 边界的上界也必须落在 [0, n) 内
(let loop
  ((i 0))
  (if (>= i 200)
    #t
    (let ((v (randbelow 257)))
      (check (>= v 0) => #t)
      (check (< v 257) => #t)
      (loop (+ i 1))
    ) ;let
  ) ;if
) ;let

;; 非法参数
(check-catch 'type-error (randbelow 3.14))
(check-catch 'type-error (randbelow "10"))
(check-catch 'type-error (randbelow #t))
(check-catch 'value-error (randbelow 0))
(check-catch 'value-error (randbelow -1))

(check-report)
