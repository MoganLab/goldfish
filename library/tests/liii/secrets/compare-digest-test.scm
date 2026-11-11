;; compare-digest
;; 常量时间比对两个字符串或两个 bytevector，防范时序旁路攻击。
;;
;; 语法
;; ----
;; (compare-digest a b)
;;
;; 参数
;; ----
;; a : string 或 bytevector
;; 待比较的第一个值。
;; b : string 或 bytevector
;; 待比较的第二个值，类型必须与 a 相同。
;;
;; 返回值
;; -----
;; boolean
;; 两者内容完全一致时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 实现不会在遇到首个差异时提前返回：完整遍历两侧内容并累积按位差异，
;;    只有在长度一致且差异累积为 0 时才返回 #t，
;;    使比较耗时与公共前缀长度无关。
;; 2. 字符串按字符编码逐个比较，bytevector 按字节逐个比较。
;;
;; 错误处理
;; ----
;; 两个参数类型不同，或不是字符串/bytevector 时抛出 type-error。

(import (liii check) (liii secrets))

(check-set-mode! 'report-failed)

;; 相同字符串
(check (compare-digest "hello-secret-token" "hello-secret-token") => #t)
(check (compare-digest "" "") => #t)

;; 内容不同的字符串
(check (compare-digest "hello-secret-token" "hello-secret-wrong") => #f)
(check (compare-digest "abc" "abd") => #f)
(check (compare-digest "abc" "ABC") => #f)

;; 长度不同的字符串
(check (compare-digest "abc" "abcd") => #f)
(check (compare-digest "abcd" "abc") => #f)
(check (compare-digest "" "a") => #f)

;; 公共前缀长度不影响结果
(check (compare-digest "aaaaaaaaaaaaaaaa" "aaaaaaaaaaaaaaaa") => #t)
(check (compare-digest "aaaaaaaaaaaaaaaa" "aaaaaaaaaaaaaaab") => #f)

;; bytevector 比对
(check (compare-digest #u8(1 2 3) #u8(1 2 3)) => #t)
(check (compare-digest #u8(1 2 3) #u8(1 2 4)) => #f)
(check (compare-digest #u8(1 2 3) #u8(1 2)) => #f)
(check (compare-digest #u8() #u8()) => #t)

;; 类型不匹配抛 type-error
(check-catch 'type-error (compare-digest "abc" #u8(1 2 3)))
(check-catch 'type-error (compare-digest #u8(1 2 3) "abc"))
(check-catch 'type-error (compare-digest 42 42))
(check-catch 'type-error (compare-digest "abc" #t))

(check-report)
