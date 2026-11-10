;; %repl-unescape-history-entry
;; 把单行文本反转义回原始条目。
;;
;; 语法
;; ----
;; (%repl-unescape-history-entry line)
;;
;; 参数
;; ----
;; line : string
;; 历史文件中的单行文本。
;;
;; 返回值
;; ----
;; string
;; 反转义之后的原始条目。

(import (scheme base) (liii check) (liii string) (liii goldrepl history))

(check-set-mode! 'report-failed)

;; \n 反转义成换行
(check (%repl-unescape-history-entry "foo\\nbar") => "foo\nbar")

;; \\ 反转义成反斜杠
(check (%repl-unescape-history-entry "foo\\\\bar") => "foo\\bar")

;; 普通文本保持不变
(check (%repl-unescape-history-entry "(+ 1 2)") => "(+ 1 2)")

;; 转义与反转义可以往返
(check (%repl-unescape-history-entry (%repl-escape-history-entry "a\\b\nc"))
  =>
  "a\\b\nc"
) ;check

(check-report)
