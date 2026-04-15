;; (liii raw-string) 模块函数分类索引
;;
;; raw-string 提供 SRFI-267 风格的原始字符串读写能力，并补充 `deindent` / `&-`
;; 这组去缩进语法，适合嵌入 HTML、SQL、JSON 和多行代码片段。

;; ==== 常见用法示例 ====
(import (liii raw-string))

;; 示例1：直接写 raw string 字面量
#"HTML"<div class="card">hello</div>"HTML"
;; => "<div class=\"card\">hello</div>"

;; 示例2：对多行 raw string 做去缩进
(&- #""
  SELECT *
  FROM users
  WHERE active = TRUE
  ""
) ;&-
(define s "<p>\"quoted\"</p>")
(define d (generate-delimiter s))
(can-delimit? s d)
