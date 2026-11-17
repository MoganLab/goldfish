;; math-block-start?
;; 判断一行文本是否是块级数学公式的起始标记。
;;
;; 语法
;; ----
;; (math-block-start? line)
;;
;; 参数
;; ----
;; line : string
;; 待判断的单行文本。
;;
;; 返回值
;; ----
;; boolean
;; 当 line 为 "$$" / "\\[" / 单行 "\\[...\\]" / 单行 "$$...$$" 时返回 #t。

(import (liii check) (liii cmark inline math))

(check-set-mode! 'report-failed)

(check-true (math-block-start? "$$"))
(check-false (math-block-start? "$$ "))
(check-false (math-block-start? "$"))

;; \[ 定界（0111 起解析器原生支持）
(check-true (math-block-start? "\\["))
(check-true (math-block-start? "\\[x+y\\]"))
(check-false (math-block-start? "\\[x"))
(check-false (math-block-start? "\\]"))

;; 单行 $$...$$ 定界（0113 起识别为块级展示公式）
(check-true (math-block-start? "$$x+y$$"))
(check-true (math-block-start? "$$E = mc^2$$"))
;; 定界间至少一个字符：$$$$ 不算单行块（$$ + $$ 由多行逻辑处理）
(check-false (math-block-start? "$$$$"))
(check-false (math-block-start? "$$x+y"))

(check-report)
