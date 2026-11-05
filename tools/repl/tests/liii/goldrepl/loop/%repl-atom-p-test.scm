;; %repl-atom?
;; 判断文本是否是原子表达式（不含括号）。
;;
;; 语法
;; ----
;; (%repl-atom? text)
;;
;; 参数
;; ----
;; text : string
;; 待判断的文本。
;;
;; 返回值
;; ----
;; boolean
;; 文本是原子表达式时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii string) (liii goldrepl loop))

(check-set-mode! 'report-failed)

;; 不含括号的表达式
(check (%repl-atom? "abc") => #t)
(check (%repl-atom? "42") => #t)
(check (%repl-atom? "\"string\"") => #t)

;; 含有括号
(check (%repl-atom? "(+ 1 2)") => #f)
(check (%repl-atom? "abc)") => #f)
(check (%repl-atom? "(abc") => #f)

;; 空文本
(check (%repl-atom? "") => #f)

(check-report)
