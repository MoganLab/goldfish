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

;; quasiquote 中点对的 cdr 位置出现 ,x 时，应保持 `(a . ,b) 形式
;; 修复前会被错误格式化为 `(a unquote b)（goldfish/liii/http.scm 等文件曾被污染）
(check (format-string "`(a . ,b)") => "`(a . ,b)\n")
(check (format-string "`((name . ,name) (file . ,spec))")
  =>
  "`((name . ,name) (file . ,spec))\n"
) ;check
;; 长前缀的点对 unquote
(check (format-string "`(a b . ,c)") => "`(a b . ,c)\n")
;; ,@ 与点对 unquote 混合
(check (format-string "`(lambda (,@xs . ,rest) (apply ,@parsed))")
  =>
  "`(lambda (,@xs . ,rest) (apply ,@parsed))\n"
) ;check
;; 反引号内的字面 (a unquote b) 与 `(a . ,b) 在 reader 层面等价（S7 的 cadr-unquote 规则），
;; 统一正规化为点对形式
(check (format-string "`(a unquote b)") => "`(a . ,b)\n")

;; 显式 quasiquote 中的点对 unquote
;; S7 reader 不展开显式 quasiquote，(quasiquote (a . ,b)) 读出为 (quasiquote (a unquote b))，
;; 与 g_quasiquote_1 的求值语义一致，恢复为点对形式
(check (format-string "(quasiquote (a . ,b))") => "`(a . ,b)\n")
(check (format-string "(quasiquote (a unquote b))") => "`(a . ,b)\n")
(check (format-string "(quasiquote (a b . ,c))") => "`(a b . ,c)\n")
(check (format-string "(quasiquote ((name . ,name) (file . ,spec)))")
  =>
  "`((name . ,name) (file . ,spec))\n"
) ;check
;; 显式 quasiquote 模板中的真实点对数据保持不变
(check (format-string "(quasiquote (a . b))") => "`(a . b)\n")

;; 普通代码中的 unquote 符号（非 quasiquote 模板上下文）不受影响
(check (format-string "(f a unquote b)") => "(f a unquote b)\n")
(check (format-string "'(a unquote b)") => "'(a unquote b)\n")

(check-report)
