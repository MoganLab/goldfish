(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; gcombine
;; 结合状态与多个生成器，每次通过 proc 产出一个值并更新状态。
;;
;; 语法
;; ----
;; (gcombine proc seed gen ...)
;;
;; 参数
;; ----
;; proc : procedure
;; 接收各生成器当前值以及当前 seed 的过程，返回两个值：(values value new-seed)。
;;
;; seed : any
;; 初始状态值。
;;
;; gen ... : procedure
;; 一个或多个输入生成器。
;;
;; 返回值
;; ----
;; procedure
;; 新生成器过程。产出 proc 返回的第一个值 value 并将状态更新为 new-seed。
;;
;; 错误处理
;; ----
;; 无

(let* ((f (lambda (item seed) (values (+ item seed) (+ seed 1))))
       (g (gcombine f 0 (generator 1 1 1))))
  (check (g) => 1)
  (check (g) => 2)
  (check (g) => 3)
  (check-true (eof-object? (g)))
)

(let* ((f (lambda (item1 item2 seed) (values (+ item1 item2 seed) (+ seed 1))))
       (g (gcombine f 0 (generator 1 1 1) (generator 2 2))))
  (check (g) => 3)
  (check (g) => 4)
  (check-true (eof-object? (g)))
)

(check-report)
