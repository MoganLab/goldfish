(import (liii random) (liii check))


(check-set-mode! 'report-failed)


;; random-integer
;; 生成 [0, n-1] 范围内的随机整数。
;;
;; 语法
;; ----
;; (random-integer n)
;;
;; 参数
;; ----
;; n : positive-integer
;; 范围上限（不包含），必须是正整数。
;;
;; 返回值
;; ----
;; integer
;; 返回 [0, n-1] 范围内的随机整数。
;;
;; 示例
;; ----
;; (random-integer 10)   => 0~9 之间的随机整数
;; (random-integer 100)  => 0~99 之间的随机整数
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 n 不是正整数时抛出。


(let ((r (random-integer 10)))
  (check (integer? r) => #t)
  (check (exact? r) => #t)
  (check (>= r 0) => #t)
  (check (< r 10) => #t)
) ;let


(check (>= (random-integer 1) 0) => #t)
(check (< (random-integer 1) 1) => #t)


(let ((r (random-integer 100)))
  (check (>= r 0) => #t)
  (check (< r 100) => #t)
) ;let


(check-catch 'wrong-type-arg
  (random-integer 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (random-integer -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (random-integer 3.14)
) ;check-catch
(check-catch 'wrong-type-arg
  (random-integer 'not-a-number)
) ;check-catch


(check-report)
