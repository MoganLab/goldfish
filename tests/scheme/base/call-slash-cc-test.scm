(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; call-with-current-continuation
;; 捕获当前续延，允许过程在之后恢复执行。
;;
;; 语法
;; ----
;; (call-with-current-continuation proc)
;; (call/cc proc)
;;
;; 参数
;; ----
;; proc : procedure?
;; 接收一个续延对象作为参数的过程。
;;
;; 返回值
;; ------
;; 任意类型
;; proc 的返回值，或续延被调用时传入的值。
;;
;; 说明
;; ----
;; 1. 续延可保存并稍后调用
;; 2. 调用续延会重置调用栈
;; 3. call/cc 是 call-with-current-continuation 的简写
(check (call/cc (lambda (k) 5)) => 5)
(check (call/cc (lambda (k) (k 10) 20)) => 10)
(check (call-with-current-continuation (lambda (k) (k 'done))) => 'done)
(let ((result (call/cc (lambda (k) (k 99) 1))))
  (check result => 99)
) ;let

(check-report)
