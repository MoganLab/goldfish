
(set! *load-path* (cons "markdown/goldfish" *load-path*))

;; (liii cmark inline math) 模块函数分类索引
;;
;; cmark-math-parser 负责识别 Markdown 扩展中的块级与行内数学公式，
;; 并把公式内容包装成 cmark 节点。

;; ==== 常见用法示例 ====
(import (liii cmark inline math))
(import (liii cmark node))

(cmark-node-type (parse-math-block '("$$" "E = mc^2" "$$")))
(cmark-node-type (parse-math-block '("\\[x-y\\]")))
(cmark-node-type (car (parse-math-inline "$x^2$" 0)))
(cmark-node-type (car (parse-math-inline "\\(x+y\\)" 0)))

;; ==== 函数分类索引 ====
;;
;; 一、块级数学公式
;;   math-block-start? - 判断某一行是否为块级公式起始标记（$$ / \[ / 单行 \[...\]）
;;   math-block-end?   - 判断某一行是否为块级公式结束标记（$$ / \]）
;;   parse-math-block  - 解析多行数学公式块
;;
;; 二、行内数学公式
;;   math-inline-start? - 判断某个位置是否是行内公式起始（$ 或 \(）
;;   parse-math-inline  - 解析行内公式并返回节点与新位置
