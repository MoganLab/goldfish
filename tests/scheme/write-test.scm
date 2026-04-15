;; (scheme write) 模块函数分类索引
;;
;; (scheme write) 提供了将 Scheme 对象输出到端口的基本 I/O 函数。
;; 包括可读性输出 (write)、用户友好展示 (display)、格式化控制 (newline, write-char)
;; 以及共享结构处理 (write-shared, write-simple) 等功能。
;; ==== 常见用法示例 ====
(import (scheme write))
;; 示例1：使用 write 输出可读形式
(define port (open-output-string))
(write '(1 2 3) port)
(get-output-string port)
;; 示例2：使用 display 输出用户友好的形式
(define port2 (open-output-string))
(display "hello" port2)
(get-output-string port2)
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/write "write"
;;   bin/gf doc scheme/write "display"
;; ==== 函数分类索引 ====
;;
;; 一、基本输出
;;   write        - 按可读回的 Scheme 表示写入对象
;;   display      - 按用户友好的方式写入对象
;;   newline      - 写入换行符
;;
;; 二、字符输出
;;   write-char   - 写入单个字符
;;
;; 三、共享结构处理
;;   write-shared - 处理循环结构的写入（当前与 write 兼容）
;;   write-simple - 简单写入（当前与 write 兼容）
