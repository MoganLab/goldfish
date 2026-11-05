;; gemini-part-text
;; 读取 Gemini part 中的文本内容。
;;
;; 语法
;; ----
;; (gemini-part-text part)
;;
;; 参数
;; ----
;; part : alist
;; Gemini 响应中的单个 part，可含 text 字段。
;;
;; 返回值
;; ----
;; string
;; part 中的文本；不存在或非字符串时返回空字符串。

(import (liii check) (liii pi ai api google-generative-ai thinking))

(check-set-mode! 'report-failed)

;; 字符串键的 text 字段
(check (gemini-part-text '(("text" . "hello"))) => "hello")

;; symbol 键的 text 字段
(check (gemini-part-text '((text . "hello"))) => "hello")

;; 与其它字段共存
(check (gemini-part-text '(("text" . "hello") ("thought" . #t))) => "hello")

;; 缺少 text 字段
(check (gemini-part-text '(("thought" . #t))) => "")

;; text 不是字符串
(check (gemini-part-text '(("text" . 42))) => "")

;; 非 alist 输入
(check (gemini-part-text #f) => "")
(check (gemini-part-text '()) => "")

(check-report)
