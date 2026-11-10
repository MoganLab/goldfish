;; %repl-escape-history-entry
;; 把条目中的反斜杠与换行转义成单行文本。
;;
;; 语法
;; ----
;; (%repl-escape-history-entry entry)
;;
;; 参数
;; ----
;; entry : string
;; 原始历史条目。
;;
;; 返回值
;; ----
;; string
;; 转义之后的单行文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl history))

(check-set-mode! 'report-failed)

;; 换行被转义成 \n
(check (%repl-escape-history-entry "foo\nbar") => "foo\\nbar")

;; 反斜杠被转义成 \\
(check (%repl-escape-history-entry "foo\\bar") => "foo\\\\bar")

;; 普通文本保持不变
(check (%repl-escape-history-entry "(+ 1 2)") => "(+ 1 2)")

;; 空条目
(check (%repl-escape-history-entry "") => "")

(check-report)
