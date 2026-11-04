;; extract-unbound-variable
;; 从未绑定变量异常参数中提取未绑定的变量名称。
;;
;; 语法
;; ----
;; (extract-unbound-variable tag info) -> string or #f
;;
;; 参数
;; ----
;; tag : symbol
;; catch 捕获到的错误标签，如 'unbound-variable。
;;
;; info : list or any
;; catch 传递给错误处理器的参数。
;;
;; 返回值
;; ----
;; string or #f
;; 成功时返回符号名称字符串，无法提取时返回 #f。

(import (liii check)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; 1. 从标准 s7 嵌套结构提取未绑定符号名
(check (extract-unbound-variable 'unbound-variable
         '(("unbound variable ~S in ~S" my-foo (+ my-foo 1)))
       ) ;extract-unbound-variable
  => "my-foo"
) ;check

;; 2. 从三层嵌套结构中提取（lambda (tag . info) 形式）
(check (extract-unbound-variable 'unbound-variable
         '((("unbound variable ~S in ~S" my-bar (+ my-bar 1))))
       ) ;extract-unbound-variable
  => "my-bar"
) ;check

;; 3. 字符串形式符号参数
(check (extract-unbound-variable 'unbound-variable
         '(("unbound variable ~S" "some-var"))
       ) ;extract-unbound-variable
  => "some-var"
) ;check

;; 4. 非 unbound-variable 标签或无效参数返回 #f
(check (extract-unbound-variable 'type-error '("bad type")) => #f)
(check (extract-unbound-variable 'unbound-variable '()) => #f)

(check-report)
