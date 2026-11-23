;; string-split-lines
;; 把字面内容按换行符拆成行列表；空字符串得到空列表。
;;
;; 语法
;; ----
;; (string-split-lines literal)
;;
;; 参数
;; ----
;; literal : string
;; 字面内容字符串。
;;
;; 返回值
;; ----
;; list
;; 行字符串列表。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? string-split-lines))

(check (string-split-lines "") => '())
(check (string-split-lines "单行") => '("单行"))
(check (string-split-lines "a\nb") => '("a" "b"))
;; 末尾换行保留为空行（代码块字面量末尾 \n 不能丢）
(check (string-split-lines "a\nb\n") => '("a" "b" ""))
(check (string-split-lines "a\n") => '("a" ""))
(check (string-split-lines "\n") => '("" ""))
(check (string-split-lines "\nabc") => '("" "abc"))

(check-report)
