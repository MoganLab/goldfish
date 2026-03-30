(import (liii check)
        (liii hashlib)
) ;import

(check-set-mode! 'report-failed)

;; md5
;; 计算字符串的 MD5 哈希值。
;;
;; 语法
;; ----
;; (md5 str)
;;
;; 参数
;; ----
;; str : string?
;; 要进行哈希计算的字符串。
;;
;; 返回值
;; -----
;; string?
;; 返回 32 位小写十六进制字符串表示的 MD5 哈希值。
;;
;; 说明
;; ----
;; 1. MD5 算法将任意长度的输入转换为 128 位（16字节）哈希值
;; 2. 返回值为 32 个十六进制字符的字符串
;; 3. 常用于数据完整性校验（但不适用于安全敏感场景）
;; 4. 空字符串有固定的哈希值

;;; 基本功能测试：常见字符串哈希
(check (md5 "") => "d41d8cd98f00b204e9800998ecf8427e")
(check (md5 "hello") => "5d41402abc4b2a76b9719d911017c592")
(check (md5 "The quick brown fox jumps over the lazy dog") => "9e107d9d372bb6826bd81d3542a419d6")
(check (md5 "a") => "0cc175b9c0f1b6a831c399e269772661")
(check (md5 "123456") => "e10adc3949ba59abbe56e057f20f883e")
(check (md5 "!@#$%^&*()") => "05b28d17a7b6e7024b6e5d8cc43a8bf7")
(check (md5 "Hello") => "8b1a9953c4611296a827abf8c47804d7")


(check-report)
