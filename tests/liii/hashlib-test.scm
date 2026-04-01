;; (liii hashlib) 模块函数分类索引
;;
;; hashlib 提供常见摘要算法的字符串和文件接口。
;; 适合校验下载文件、生成缓存键、检测内容变化和生成指纹。

;; ==== 常见用法示例 ====
(import (liii hashlib))

;; 示例1：计算字符串的 MD5 摘要
(md5 "goldfish")

;; 示例2：计算字符串的 SHA-1 摘要
(sha1 "goldfish")

;; 示例3：计算字符串的 SHA-256 摘要
(sha256 "goldfish")

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/hashlib "md5"
;;   bin/gf doc liii/hashlib "sha256-by-file"

;; ==== 函数分类索引 ====

;; 一、字符串摘要
;; 用于对字符串内容计算摘要值的函数
;;   md5             - 计算字符串的 MD5 摘要
;;   sha1            - 计算字符串的 SHA-1 摘要
;;   sha256          - 计算字符串的 SHA-256 摘要

;; 二、文件摘要
;; 用于对文件内容计算摘要值的函数
;;   md5-by-file     - 计算文件的 MD5 摘要
;;   sha1-by-file    - 计算文件的 SHA-1 摘要
;;   sha256-by-file  - 计算文件的 SHA-256 摘要
