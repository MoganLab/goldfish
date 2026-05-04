(import (liii check))

;; 测试 exit-hook 基本功能
(check (procedure? *exit-hook*) => #t)

;; 测试 hook-functions 设置
(define *exit-called* #f)
(define *exit-code* #f)

(set! (hook-functions *exit-hook*)
  (cons (lambda (hook)
          (set! *exit-called* #t)
          (set! *exit-code* (hook 'code)))
        (hook-functions *exit-hook*)))

;; 不能直接测试 exit 触发，因为 exit 会结束进程
;; 但我们可以验证 hook-functions 已经被设置
(check (pair? (hook-functions *exit-hook*)) => #t)

(check-report)
