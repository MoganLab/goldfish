(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; call-with-current-continuation
;; 捕获当前续延，允许过程在之后恢复执行。
;;
;; call-with-current-continuation 与 call/cc 是同一个过程的不同名称，
;; 两者行为完全相同。详细测试用例和更多示例请参考 gf doc "call/cc"。
;;
;; 语法
;; ----
;; (call-with-current-continuation proc)
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
;; 1. call-with-current-continuation 是 call/cc 的完整名称
;; 2. 两者行为完全相同，只是名称长度不同
;; 3. call/cc 更为常用，尤其在需要频繁使用的场景

(check (call-with-current-continuation (lambda (k) 5)) => 5)
(check (call-with-current-continuation (lambda (k) (k 'done))) => 'done)

(check-report)
