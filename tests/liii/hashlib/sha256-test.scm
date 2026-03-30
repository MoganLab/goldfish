(import (liii check)
        (liii hashlib)
) ;import

(check-set-mode! 'report-failed)

;; sha256
;; 计算字符串的 SHA256 哈希值。
;;
;; 语法
;; ----
;; (sha256 str)
;;
;; 参数
;; ----
;; str : string?
;; 要进行哈希计算的字符串。
;;
;; 返回值
;; -----
;; string?
;; 返回 64 位小写十六进制字符串表示的 SHA256 哈希值。
;;
;; 说明
;; ----
;; 1. SHA256 算法将任意长度的输入转换为 256 位（32字节）哈希值
;; 2. 返回值为 64 个十六进制字符的字符串
;; 3. 比 SHA1 更安全，推荐用于安全敏感场景
;; 4. 空字符串有固定的哈希值

;;; 基本功能测试：常见字符串哈希
(check (sha256 "") => "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
(check (sha256 "hello") => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
(check (sha256 "The quick brown fox jumps over the lazy dog") => "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
(check (sha256 "a") => "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
(check (sha256 "123456") => "8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92")
(check (sha256 "!@#$%^&*()") => "95ce789c5c9d18490972709838ca3a9719094bca3ac16332cfec0652b0236141")
(check (sha256 "Hello") => "185f8db32271fe25f561a6fc938b2e264306ec304eda518007d1764826381969")


(check-report)
