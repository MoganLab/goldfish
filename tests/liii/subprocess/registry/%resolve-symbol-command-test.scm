;; %resolve-symbol-command
;; 解析符号命令并根据黑白名单进行安全校验。
;;
;; 语法
;; ----
;; (%resolve-symbol-command sym args)
;;
;; 参数
;; ----
;; sym : symbol
;; 命令符号。
;;
;; args : list
;; 传递给命令的参数列表。
;;
;; 返回值
;; ----
;; list
;; 解析后的命令列表，首元素为 lambda 表达式、可执行路径或字符串命令名。
;;
;; 说明
;; ----
;; 校验 sym 是否在黑名单中或不在白名单中，若违规则抛出 value-error；通过后依据注册表映射返回命令列表。

(import (liii check) (liii error) (liii path) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 1. 测试注册 procedure

(define my-fn (lambda (x y) (+ x y)))
(run-set! 'my-fn my-fn)
(let ((res (%resolve-symbol-command 'my-fn '(1 2))))
  (check (car res) => 'lambda)
  (check (cadr res) => my-fn)
  (check (cddr res) => '(1 2))
) ;let

;; 2. 测试注册 string
(run-set! 'my-sh "/bin/sh")
(check (%resolve-symbol-command 'my-sh '("-c" "true"))
  =>
  '("/bin/sh" "-c" "true")
) ;check

;; 3. 测试注册 path
(run-set! 'my-path (path "/usr/bin/env"))
(check (%resolve-symbol-command 'my-path '()) => '("/usr/bin/env"))

;; 4. 测试未注册符号
(check (%resolve-symbol-command 'unknown-cmd '()) => '("unknown-cmd"))
(check (%resolve-symbol-command 'unknown-cmd '("arg1" "arg2"))
  =>
  '("unknown-cmd" "arg1" "arg2")
) ;check

;; 5. 测试被 ban 的符号（如 'rm）抛出 value-error
(check-catch 'value-error (%resolve-symbol-command 'rm '("-rf" "/")))

;; 6. 测试白名单开启时，不在白名单中的符号抛出 value-error
(run-allow! '(my-sh))
(check (%resolve-symbol-command 'my-sh '("-c" "true"))
  =>
  '("/bin/sh" "-c" "true")
) ;check
(check-catch 'value-error (%resolve-symbol-command 'my-path '()))
(check-catch 'value-error (%resolve-symbol-command 'unknown-cmd '()))

;; 清空恢复白名单
(run-allow! '())

(check-report)
