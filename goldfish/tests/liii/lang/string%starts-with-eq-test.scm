;; string%starts-with? / string%ends-with?
;; 判断字符串是否以指定前缀/后缀开始或结尾（对标 Scala String.startsWith / endsWith）。
;;
;; 语法
;; ----
;; ($ s :starts-with? prefix)
;; ($ s :ends-with? suffix)
;;
;; 参数
;; ----
;; prefix : string?
;; 前缀字符串。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; boolean
;; 前缀/后缀匹配返回 #t；prefix/suffix 为空串恒 #t。
;;
;; 描述
;; ----
;; 基于 (liii string-cursor) 的 string-prefix? 与 string-suffix?，UTF-8 字符序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "hello" :starts-with? "he") => #t)
(check ($ "hello" :starts-with? "") => #t)
(check ($ "hello" :starts-with? "lo") => #f)
(check ($ "测试" :starts-with? "测") => #t)

(check ($ "hello" :ends-with? "lo") => #t)
(check ($ "hello" :ends-with? "") => #t)
(check ($ "hello" :ends-with? "he") => #f)
(check ($ "测试" :ends-with? "试") => #t)

(check-report)
