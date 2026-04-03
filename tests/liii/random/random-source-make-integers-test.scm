(import (liii random)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; random-source-make-integers
;; 从随机源创建整数生成器。
;;
;; 语法
;; ----
;; (random-source-make-integers s)
;;
;; 参数
;; ----
;; s : random-source
;; 用于生成随机数的随机源。
;;
;; 返回值
;; ----
;; procedure
;; 返回一个过程 (lambda (n) ...)，生成 [0, n-1] 的随机整数。
;;
;; 示例
;; ----
;; (let* ((s (make-random-source))
;;        (rand-int (random-source-make-integers s)))
;;   (rand-int 100))  => 0~99 之间的随机整数
;;
;; 错误处理
;; ----
;; wrong-type-arg 当 s 不是随机源时抛出。

; 创建整数生成器
(let* ((s (make-random-source))
       (rand-int (random-source-make-integers s)))
  (check (procedure? rand-int) => #t)
)

; 生成器工作正常
(let* ((s (make-random-source))
       (rand-int (random-source-make-integers s)))
  (let ((r (rand-int 100)))
    (check (integer? r) => #t)
    (check (>= r 0) => #t)
    (check (< r 100) => #t)
  )
)

; 多次调用
(let* ((s (make-random-source))
       (rand-int (random-source-make-integers s)))
  (let ((r1 (rand-int 100))
        (r2 (rand-int 100))
        (r3 (rand-int 100)))
    (check (integer? r1) => #t)
    (check (integer? r2) => #t)
    (check (integer? r3) => #t)
  )
)

; 错误处理
(check-catch 'wrong-type-arg (random-source-make-integers 'not-a-source))

(check-report)
