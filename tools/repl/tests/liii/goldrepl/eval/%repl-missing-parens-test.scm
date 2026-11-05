;; %repl-missing-parens
;; 统计文本中未闭合的括号数量，字符串与注释内容不参与统计。
;;
;; 语法
;; ----
;; (%repl-missing-parens text)
;;
;; 参数
;; ----
;; text : string
;; 待检查的输入文本。
;;
;; 返回值
;; ----
;; integer
;; 未闭合的左括号数量；右括号多于左括号时返回负数。

(import (scheme base) (liii check) (liii goldrepl eval))

(check-set-mode! 'report-failed)

;; 括号配对
(check (%repl-missing-parens "(+ 1 2)") => 0)

;; 缺少一个右括号
(check (%repl-missing-parens "(define (f x)") => 1)

;; 缺少两个右括号
(check (%repl-missing-parens "(define (f x) (let ((y 1))") => 2)

;; 右括号多于左括号
(check (%repl-missing-parens ")") => -1)

;; 字符串中的括号不参与统计
(check (%repl-missing-parens "(string-append \")\" \"(\")") => 0)

;; 注释中的括号不参与统计
(check (%repl-missing-parens "(+ 1 2) ; unclosed parenthesis: (") => 0)

(check-report)
