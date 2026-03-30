(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; char<?
;; 按字典序比较字符的大小，判断字符是否按升序排列。
;;
;; 语法
;; ----
;; (char<? char1 char2 char3 ...)
;;
;; 参数
;; ----
;; char1, char2, char3, ... : char?
;; 要比较的字符，至少需要两个。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符按升序排列（即每个字符都小于下一个字符）则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须都是字符
;; 3. 按字符的Unicode码点值进行比较
;; 4. 当字符按严格升序排列时返回 #t，否则返回 #f
;; 5. 区分大小写，大写字符码点值小于小写字符（如 #\A < #\a）
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。

;; char<? 基本测试
(check (char<? #\A #\B) => #t)
(check (char<? #\a #\b) => #t)
(check (char<? #\A #\a) => #t)  ; 大写小于小写
(check (char<? #\a #\A) => #f)  ; 小写不小于大写
(check (char<? #\0 #\9) => #t)
(check (char<? #\9 #\0) => #f)

;; 相等字符测试
(check (char<? #\A #\A) => #f)
(check (char<? #\a #\a) => #f)
(check (char<? #\0 #\0) => #f)

;; 特殊字符测试
(check (char<? #\space #\newline) => #f)
(check (char<? #\tab #\space) => #t)
(check (char<? #\newline #\tab) => #f)

;; 多参数升序测试
(check (char<? #\A #\B #\C) => #t)
(check (char<? #\a #\b #\c) => #t)
(check (char<? #\0 #\1 #\2 #\3 #\4) => #t)
(check (char<? #\! #\# #\$ #\%) => #t)

;; 多参数非升序测试
(check (char<? #\A #\B #\A) => #f)
(check (char<? #\a #\a #\b) => #f)  ; 等于不满足小于关系
(check (char<? #\3 #\2 #\1) => #f)

;; 混合大小写测试
(check (char<? #\A #\a #\b) => #t)
(check (char<? #\Z #\a #\z) => #t)
(check (char<? #\a #\Z #\b) => #f)

;; 边界测试
(check (char<? #\0 #\9) => #t)
(check (char<? #\A #\Z) => #t)
(check (char<? #\a #\z) => #t)
(check (char<? #\! #\~) => #t)

;; 数字字符测试
(check (char<? #\1 #\2) => #t)
(check (char<? #\5 #\5) => #f)
(check (char<? #\9 #\8) => #f)

;; 错误处理测试
(check-catch 'wrong-type-arg (char<? 1 #\A))
(check-catch 'wrong-type-arg (char<? #\A 'symbol))
(check-catch 'wrong-type-arg (char<? 123 #\a))
(check-catch 'wrong-number-of-args (char<?))
(check-catch 'wrong-number-of-args (char<? #\A))

(check-report)
