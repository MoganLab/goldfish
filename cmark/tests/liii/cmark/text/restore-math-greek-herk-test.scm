;; restore-math-greek-herk
;; 将数学容器内 utf8->herk 产生的 <#3C6>/<#3D5>/<#3D6> 还原为 <varphi>/<phi>/<varpi>。
;;
;; 语法
;; ----
;; (restore-math-greek-herk s)
;;
;; 参数
;; ----
;; s : string
;; 包含 TeXmacs herk 编码希腊字母的字符串。
;;
;; 返回值
;; ----
;; string
;; 还原后的字符串。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (restore-math-greek-herk "<#3C6>") => "<varphi>")
(check (restore-math-greek-herk "<#03C6>") => "<varphi>")
(check (restore-math-greek-herk "<#3D5>") => "<phi>")
(check (restore-math-greek-herk "<#03D5>") => "<phi>")
(check (restore-math-greek-herk "<#3D6>") => "<varpi>")
(check (restore-math-greek-herk "<#03D6>") => "<varpi>")
(check (restore-math-greek-herk "<#3C6>*<#3D6>*<#3D5>=1")
  =>
  "<varphi>*<varpi>*<phi>=1"
) ;check
(check (restore-math-greek-herk "plain text <#3B1>") => "plain text <#3B1>")

(check-report)
