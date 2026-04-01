;; (liii raw-string) 模块函数分类索引
;;
;; liii raw-string 提供原始字符串的缩进处理功能，主要用于移除多行字符串
;; 中不必要的缩进空格，使代码保持整洁的缩进同时得到正确的字符串内容。

;; ==== 常见用法示例 ====
(import (liii raw-string))

;; 示例1：使用 &- 宏处理带缩进的多行字符串
;; (&- #""
;;   第一行
;;     第二行（缩进2格）
;;   第三行
;;   "")
;; => "第一行\n  第二行（缩进2格）\n第三行"

;; 示例2：使用自定义分隔符的原始字符串
;; (&- #"HTML"
;;   <div>
;;     <p>内容</p>
;;   </div>
;;   "HTML")
;; => "<div>\n  <p>内容</p>\n</div>"

;; 示例3：直接在代码中嵌入 SQL/HTML/JSON 等多行文本
;; (define query (&- #""
;;   SELECT *
;;   FROM users
;;   WHERE active = TRUE
;;   ""))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/raw-string "function-name"

;; ==== 函数分类索引 ====
;;
;; 一、原始字符串缩进处理
;;   deindent  - 移除原始字符串的缩进空格
;;   &-        - deindent 的别名，更简洁的写法
