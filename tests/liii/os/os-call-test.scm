(import (liii check) (liii os) (scheme time))


(check-set-mode! 'report-failed)


;; os-call
;; 执行系统命令。
;;
;; 语法
;; ----
;; (os-call command)
;;
;; 参数
;; ----
;; command : string?
;; 要执行的系统命令字符串。
;;
;; 说明
;; ----
;; 执行指定的系统命令并等待其完成。


;; ; 基本功能测试
(when (not (os-windows?))
  (let ((t1 (current-second)))
    (os-call "sleep 1")
    (let ((t2 (current-second)))
      (check (>= (ceiling (- t2 t1)) 1) => #t)
    ) ;let
  ) ;let
) ;when


;; ; 参数类型测试
;; command 必须是 string?，传入其他类型应报 wrong-type-arg
;; 而不是把整数等对象当作 C 字符串指针导致段错误 (devel/0142.md)
(check-catch 'wrong-type-arg (os-call 42))
(check-catch 'wrong-type-arg (os-call 'ls))
(check-catch 'wrong-type-arg (os-call #t))
(check-catch 'wrong-type-arg (os-call (list "ls")))
;; C 层入口 g_os-call 走同一实现，同样需要类型检查
(check-catch 'wrong-type-arg (g_os-call 42))


(check-report)
