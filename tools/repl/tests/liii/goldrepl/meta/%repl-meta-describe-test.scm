;; %repl-meta-describe
;; 描述符号的类型、参数个数与文档，返回描述行列表。
;;
;; 语法
;; ----
;; (%repl-meta-describe arg)
;;
;; 参数
;; ----
;; arg : string
;; 待描述的符号名。
;;
;; 返回值
;; ----
;; list
;; 描述信息的行列表；符号不存在时返回一行提示信息。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl meta)
) ;import

(check-set-mode! 'report-failed)

;; 空参数返回用法提示
(let ((lines (%repl-meta-describe "")))
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "Usage:"))
) ;let

;; 未定义的符号
(let ((lines (%repl-meta-describe "no-such-symbol-xyz")))
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "Symbol not defined:"))
) ;let

;; 描述过程：包含类型与参数个数
(let ((lines (%repl-meta-describe "car")))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "procedure")) lines))
  (check-true (any (lambda (l) (string-contains l "Arity")) lines))
) ;let

;; 描述变量：包含取值预览
(eval '(define *goldrepl-meta-var* 42) (rootlet))
(let ((lines (%repl-meta-describe "*goldrepl-meta-var*")))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Value:")) lines))
) ;let

(check-report)
