(import (liii check)
        (liii hashlib)
) ;import

(check-set-mode! 'report-failed)

;; sha1
;; 计算字符串的 SHA1 哈希值。
;;
;; 语法
;; ----
;; (sha1 str)
;;
;; 参数
;; ----
;; str : string?
;; 要进行哈希计算的字符串。
;;
;; 返回值
;; -----
;; string?
;; 返回 40 位小写十六进制字符串表示的 SHA1 哈希值。
;;
;; 说明
;; ----
;; 1. SHA1 算法将任意长度的输入转换为 160 位（20字节）哈希值
;; 2. 返回值为 40 个十六进制字符的字符串
;; 3. 比 MD5 更安全，但也不推荐用于安全敏感场景
;; 4. 空字符串有固定的哈希值

;;; 基本功能测试：常见字符串哈希
(check (sha1 "") => "da39a3ee5e6b4b0d3255bfef95601890afd80709")
(check (sha1 "hello") => "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d")
(check (sha1 "The quick brown fox jumps over the lazy dog") => "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
(check (sha1 "a") => "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")
(check (sha1 "123456") => "7c4a8d09ca3762af61e59520943dc26494f8941b")
(check (sha1 "!@#$%^&*()") => "bf24d65c9bb05b9b814a966940bcfa50767c8a8d")
(check (sha1 "Hello") => "f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0")


(check-report)
