;; %resolve-symbol-command
;; 解析符号命令为底层可执行形式，并进行黑白名单安全校验。
;;
;; 语法
;; ----
;; (%resolve-symbol-command sym args)
;;
;; 参数
;; ----
;; sym : symbol
;; 要解析的命令符号。
;;
;; args : list
;; 传递给该命令的参数列表。
;;
;; 返回值
;; ----
;; list
;; 解析后的命令列表。如果注册值为 procedure 则前缀 lambda，如果为 string 或 path 则返回路径字符串与参数列表，未注册符号则直接转为字符串与参数列表。
;;
;; 说明
;; ----
;; 若符号命令命中黑名单或未在开启的白名单中，将抛出 value-error。

(import (liii check) (liii error) (liii path) (liii subprocess registry))

(check-set-mode! 'report-failed)

;; 1. 注册 procedure
(let ((my-proc (lambda (a b) (+ a b))))
  (run-set! 'my-fn my-proc)
  (check (%resolve-symbol-command 'my-fn '(1 2)) => (list 'lambda my-proc 1 2))
) ;let

;; 2. 注册 string
(run-set! 'my-sh "/bin/sh")
(check (%resolve-symbol-command 'my-sh '("-c" "true"))
  =>
  '("/bin/sh" "-c" "true")
) ;check

;; 3. 注册 path
(let ((p (path "/usr/bin/env")))
  (run-set! 'my-path p)
  (check (%resolve-symbol-command 'my-path '()) => (list (path->string p)))
) ;let

;; 4. 未注册符号
(check (%resolve-symbol-command 'unknown-cmd '()) => '("unknown-cmd"))
(check (%resolve-symbol-command 'custom-cmd '("arg1" "arg2"))
  =>
  '("custom-cmd" "arg1" "arg2")
) ;check

;; 5. 被 ban 的符号（如默认的 'rm）
(check-catch 'value-error (%resolve-symbol-command 'rm '("-rf" "/tmp/test")))

;; 6. 白名单开启时，不在白名单中的符号被拦截
(run-allow! '(my-sh))
(check (%resolve-symbol-command 'my-sh '("-c" "true"))
  =>
  '("/bin/sh" "-c" "true")
) ;check
(check-catch 'value-error (%resolve-symbol-command 'unknown-cmd '()))
(check-catch 'value-error (%resolve-symbol-command 'my-path '()))

;; 清空白名单以防影响其他测试
(run-allow! '())
(check (%resolve-symbol-command 'unknown-cmd '()) => '("unknown-cmd"))

(check-report)
