;; %repl-register-value!
;; 把求值结果注册为 $序号 形式的会话变量，并返回该变量名。
;;
;; 语法
;; ----
;; (%repl-register-value! result)
;;
;; 参数
;; ----
;; result : any
;; 需要注册的求值结果。
;;
;; 返回值
;; ----
;; symbol
;; 形如 `$1`、`$2` 的会话变量名。

(import (scheme base)
  (scheme repl)
  (liii check)
  (liii string)
  (liii goldrepl eval)
) ;import

(check-set-mode! 'report-failed)

;; 返回的是带序号的符号
(let ((sym (%repl-register-value! 123)))
  (check (symbol? sym) => #t)
  (check (string-starts? (symbol->string sym) "$") => #t)
) ;let

;; 注册之后可以从交互环境中取回该值
(let ((sym (%repl-register-value! 42)))
  (check (eval sym (interaction-environment)) => 42)
) ;let

;; 多次注册得到不同的变量名
(let* ((sym1 (%repl-register-value! 1)) (sym2 (%repl-register-value! 2)))
  (check (not (eq? sym1 sym2)) => #t)
  (check (eval sym1 (interaction-environment)) => 1)
  (check (eval sym2 (interaction-environment)) => 2)
) ;let*

(check-report)
