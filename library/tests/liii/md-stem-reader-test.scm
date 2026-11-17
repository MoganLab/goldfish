
(set! *load-path* (cons "markdown/goldfish" *load-path*))

;; (liii md-stem-reader) 模块函数分类索引
;;
;; md-stem-reader 负责读取 `.md.stem` 资源文本，并返回
;; 完整的 `(md-stem ...)` 文档结构。

;; ==== 常见用法示例 ====
(import (liii md-stem-reader))

(read-md-stem (open-input-string "(p \"正文\")"))

;; ==== 函数分类索引 ====
;;
;; 一、读取接口
;;   read-md-stem - 读取 md.stem 资源文本为 S 表达式文档
