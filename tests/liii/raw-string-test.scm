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
  "")
;; => "SELECT *\nFROM users\nWHERE active = TRUE"

;; 示例3：为任意字符串生成合适的 delimiter
(define s "<p>\"quoted\"</p>")
(define d (generate-delimiter s))
(can-delimit? s d) ; => #t

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/raw-string "deindent"
;;   bin/gf doc liii/raw-string "write-raw-string"
;;
;; 具体断言测试见：
;;   tests/liii/raw-string/raw-string-literal-test.scm
;;   tests/liii/raw-string/deindent-test.scm
;;   tests/liii/raw-string/ampersand-minus-test.scm

;; ==== 函数分类索引 ====

;; 一、Reader 语法
;; 用于直接读取 SRFI-267 风格原始字符串的语法入口
;;   #"...             - 读取 raw string 字面量

;; 二、SRFI-267 读写过程
;; 用于检查错误、读取原始字符串和生成可写出的 delimiter
;;   raw-string-read-error?       - 判断是否为 raw string 读取错误
;;   raw-string-write-error?      - 判断是否为 raw string 写出错误
;;   read-raw-string              - 从端口读取 raw string
;;   read-raw-string-after-prefix - 在读过 `#"` 后继续读取 raw string
;;   can-delimit?                 - 判断某个 delimiter 是否可用于字符串
;;   generate-delimiter           - 为字符串生成可用 delimiter
;;   write-raw-string             - 将字符串按 raw string 形式写出

;; 三、去缩进语法
;; 用于处理多行 raw string 的公共缩进
;;   deindent                     - 对多行字符串执行去缩进
;;   &-                           - `deindent` 的别名语法
