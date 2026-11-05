;; %resolve-symbol-command
;; 解析符号命令为最终执行形式与参数列表。
;;
;; 语法
;; ----
;; (%resolve-symbol-command sym args)
;;
;; 参数
;; ----
;; sym : symbol
;; 符号命令名。
;;
;; args : list
;; 传递给命令的参数列表。
;;
;; 返回值
;; ------
;; list
;; 解析后的命令与参数列表。
;;
;; 说明
;; ----
;; - 首先通过 %check-symbol-command 进行黑白名单校验。
;; - 若符号注册为 procedure，解析为 (cons 'lambda (cons val args))。
;; - 若符号注册为 string，解析为 (cons val args)。
;; - 若符号注册为 path 对象，解析为 (cons (path->string val) args)。
;; - 若未注册，fallback 为 (cons (symbol->string sym) args)。

(import (liii check) (liii path) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 黑名单中符号校验失败
(check-catch 'value-error (%resolve-symbol-command 'rm '("-rf" "/tmp/test")))

;; procedure 分支展开

(define test-proc (lambda (x) x))
(run-set! 'my-proc test-proc)
(check (%resolve-symbol-command 'my-proc '("arg1" "arg2"))
  =>
  (cons 'lambda (cons test-proc '("arg1" "arg2")))
) ;check

;; string 分支展开
(run-set! 'my-str "/bin/echo")
(check (%resolve-symbol-command 'my-str '("hello")) => '("/bin/echo" "hello"))

;; path 分支展开
(run-set! 'my-path (path "/bin/true"))
(check (%resolve-symbol-command 'my-path '())
  =>
  (list (path->string (path "/bin/true")))
) ;check

;; 未注册符号 fallback
(check (%resolve-symbol-command 'unknown-cmd '("foo" "bar"))
  =>
  '("unknown-cmd" "foo" "bar")
) ;check

(check-report)
