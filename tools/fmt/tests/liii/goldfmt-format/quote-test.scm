(import (liii check) (liii goldfmt-format))

(check-set-mode! 'report-failed)

;; 测试 ''(a b) 应被格式化为 ''(a b)，而不是 '(quote (a b)) 或 '(#_quote (a b))
(check (format-string "''(a b)") => "''(a b)\n")

;; 测试列表内部的 quote 形式应格式化为 'x
(check (format-string "(list '(a b))") => "(list '(a b))\n")

;; 测试 `,@ 形式应被格式化为 `,@，而不是展开为 S7 内部形式
(check (format-string "`(a ,@b)") => "`(a ,@b)\n")
(check (format-string "`(a ,b ,@c)") => "`(a ,b ,@c)\n")

;; 多点对中的 ,@
(check (format-string "`(a ,@b . c)") => "`(a ,@b . c)\n")
;; define-macro 中的 ,@
(check (format-string "(define-macro (m x) `(list ,@x))")
  =>
  "(define-macro (m x) `(list ,@x))\n"
) ;check
;; ,@ 后面紧跟右括号
(check (format-string "`(,@a)") => "`(,@a)\n")
;; ,@ 后面紧跟符号
(check (format-string "`(,@a b)") => "`(,@a b)\n")
;; ,@ 后面跟函数调用形式
(check (format-string "`(a ,@(f x))") => "`(a ,@(f x))\n")
(check (format-string "`(,@(maxima-launchers))") => "`(,@(maxima-launchers))\n")
;; ,@ 出现在显式 (quasiquote ...) 形式中
;; 与 (quasiquote (a b)) => `(a b) 的既有行为一致，显式 quasiquote 会转换为 ` 形式
(check (format-string "(quasiquote (a ,@(maxima-launchers)))")
  =>
  "`(a ,@(maxima-launchers))\n"
) ;check
;; 确认不含 ,@ 的显式 quasiquote 的既有行为
(check (format-string "(quasiquote (a b))") => "`(a b)\n")
(check (format-string "(quasiquote (a ,b))") => "`(a ,b)\n")

(check-report)
