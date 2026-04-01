;; (scheme file) 模块函数分类索引
;;
;; (scheme file) 提供 R7RS 文件操作函数，支持文件的读写、存在性检查和加载。
;; 该库是 scheme 标准库的一部分，与 (liii os) 相比更专注于 Scheme 标准文件操作。

;; ==== 常见用法示例 ====
;; (import (scheme file))

;; 示例1：写入并读取文件
;; (with-output-to-file "output.txt"
;;   (lambda () (display "Hello, World!")))
;;
;; (call-with-input-file "output.txt"
;;   (lambda (port) (read-line port)))

;; 示例2：检查文件是否存在并删除
;; (when (file-exists? "old.txt")
;;   (delete-file "old.txt"))

;; 示例3：加载并执行 Scheme 代码文件
;; (load "my-script.scm")

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/file "function-name"

;; ==== 函数分类索引 ====
;;
;; 一、文件输出
;;   with-output-to-file       - 将输出重定向到文件
;;
;; 二、文件输入
;;   call-with-input-file      - 用输入文件调用函数
;;
;; 三、文件操作
;;   file-exists?              - 检查文件是否存在
;;   delete-file               - 删除文件
;;   load                      - 加载并执行 Scheme 文件
