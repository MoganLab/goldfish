(import (liii check) (liii goldfmt-scan) (liii goldfmt-format))

(check-set-mode! 'report-failed)

;; stem 模式：TeXmacs .stem 文件中的 quote/quasiquote/unquote/unquote-splicing
;; 是普通的过程/语法符号，不是 Scheme reader 语法，格式化时必须保持原样，
;; 不能糖化为 ' ` , ,@（TeXmacs 无法识别这些写法，改写会损坏文件）。
;;
;; 约定（方案A：结构原样）：源码中的 'x / ,x 字面形式在 stem 模式下
;; 统一输出为 (quote x) / (unquote x) 结构形式。

(define (format-string-stem source)
  (call-with-stem-mode (lambda () (format-string source)))
) ;define

;; (quote x) 保持原样，不转换为 'x
(check (format-string-stem "(quote x)") => "(quote x)\n")
(check (format-string-stem "(quote (a b))") => "(quote (a b))\n")

;; (unquote x) 保持原样，不转换为 ,x
(check (format-string-stem "(unquote x)") => "(unquote x)\n")
(check (format-string-stem "(unquote (arg \"env\"))")
  =>
  "(unquote (arg \"env\"))\n"
) ;check

;; (unquote-splicing x) 保持原样，不转换为 ,@x
(check (format-string-stem "(unquote-splicing x)") => "(unquote-splicing x)\n")

;; (quasiquote x) 保持原样，不转换为 `x
(check (format-string-stem "(quasiquote (a b))") => "(quasiquote (a b))\n")

;; 显式 quasiquote 模板中的 unquote 符号保持原样
;; scm 模式下 (quasiquote (a unquote b)) 会被正规化为 `(a . ,b)，
;; stem 模式下整个形式按普通列表处理，模板符号原样保留
(check (format-string-stem "(quasiquote (a unquote b))")
  =>
  "(quasiquote (a unquote b))\n"
) ;check

;; env-base.stem 真实场景：quasi + 嵌套 unquote（超长行触发换行布局）
(check (format-string-stem "(quasi (concat (add-to-counter-group (unquote (arg \"env\")) (unquote (arg \"grp\")))))"
       ) ;format-string-stem
  =>
  "(quasi (concat (add-to-counter-group (unquote (arg \"env\")) (unquote (arg \"grp\"))))\n) ;quasi\n"
) ;check

;; 嵌套列表中的 quote/unquote 保持原样
(check (format-string-stem "(assign \"x\" (macro (unquote (arg \"env\"))))")
  =>
  "(assign \"x\" (macro (unquote (arg \"env\"))))\n"
) ;check

;; 结构原样：源码中的 'x 统一输出为 (quote x)
(check (format-string-stem "'x") => "(quote x)\n")
(check (format-string-stem "'(a b)") => "(quote (a b))\n")
(check (format-string-stem "''(a b)") => "(quote (quote (a b)))\n")

;; 结构原样：源码中的 `x 统一输出为 (quasiquote x)
(check (format-string-stem "`(a ,b)") => "(quasiquote (a (unquote b)))\n")

;; 幂等：格式化输出再次格式化结果不变
(check (format-string-stem (format-string-stem "(quasi (unquote x))"))
  =>
  "(quasi (unquote x))\n"
) ;check

;; scm 模式不受影响：quote/unquote 仍然糖化（回归保证）
(check (format-string "(quote x)") => "'x\n")
(check (format-string "(quasiquote (a ,b))") => "`(a ,b)\n")

(check-report)
