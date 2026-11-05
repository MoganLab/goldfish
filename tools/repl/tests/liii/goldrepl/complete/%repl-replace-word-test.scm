;; %repl-replace-word
;; 替换光标左侧的当前词，并保留其余文本。
;;
;; 语法
;; ----
;; (%repl-replace-word text col word replacement)
;;
;; 参数
;; ----
;; text : string
;; 编辑器中的当前文本。
;; col : integer
;; 光标所在的列号。
;; word : string
;; 光标左侧的当前词。
;; replacement : string
;; 用于替换当前词的新文本。
;;
;; 返回值
;; ----
;; string
;; 替换之后的完整文本。

(import (scheme base) (liii check) (liii goldrepl complete))

(check-set-mode! 'report-failed)

;; 替换词并保留其余部分
(check (%repl-replace-word "(map string- 123)" 12 "string-" "string-append")
  =>
  "(map string-append 123)"
) ;check

;; 光标位置超出文本长度时按文本末尾处理
(check (%repl-replace-word "abc" 100 "abc" "xyz") => "xyz")

;; 词左侧为空时在文本头部插入
(check (%repl-replace-word "(abc" 0 "" "x") => "x(abc")

(check-report)
