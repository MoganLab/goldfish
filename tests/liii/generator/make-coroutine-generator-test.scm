(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; make-coroutine-generator
;; 基于协程过程构造生成器。协程通过调用传入的 yield 产出元素。
;;
;; 语法
;; ----
;; (make-coroutine-generator proc)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收一个 yield 参数的单参过程。过程内部可通过 (yield v) 产出值。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。每次调用恢复执行 proc 直至下一次 yield；proc 结束时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (make-coroutine-generator (lambda (yield) (yield 10) (yield 20)))))
  (check (g) => 10)
  (check (g) => 20)
  (check-true (eof-object? (g)))
  (check-true (eof-object? (g)))
)

(let* ((counter 0)
       (g (make-coroutine-generator
            (lambda (yield)
              (set! counter (+ counter 1))
              (yield counter)
              (set! counter (+ counter 1))
              (yield counter)))))
  (check counter => 0)
  (check (g) => 1)
  (check counter => 1)
  (check (g) => 2)
  (check counter => 2)
  (check-true (eof-object? (g)))
)

(check-report)
