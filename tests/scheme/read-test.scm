;; (scheme read) 模块函数分类索引
;;
;; (scheme read) 提供 R7RS 读取函数，用于从输入端口解析 Scheme 数据。
;; 该库是 scheme 标准库的一部分，是 Scheme 解释器的基础功能。
;; ==== 常见用法示例 ====
;; (import (scheme read))
;; 示例1：从字符串读取数据
;; (with-input-from-string "(+ 1 2)"
;; (lambda () (read)))
;; ; => (+ 1 2)
;; 示例2：从输入端口读取
;; (let ((port (open-input-string "\"hello\"")))
;; (read port))
;; ; => "hello"
;; ==== 如何查看函数的文档和用例 ====
;; bin/gf doc scheme/read "function-name"
;; ==== 函数分类索引 ====
;;
;; 一、数据读取
;; read         - 从输入端口读取 Scheme datum