;; string%find
;; 查找字符串中首个满足谓词的字符（对标 Scala String.find）。
;;
;; 语法
;; ----
;; ($ s :find p)
;;
;; 参数
;; ----
;; p : procedure?
;; 字符谓词；可用 `_` 占位符简写。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; (option ...)
;; (option 首个满足字符)；不存在返回 (none)。UTF-8 语义。

(import (liii check) (liii lang) (liii option) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "ab" :find (char=? _ #\b)) => (option #\b))
(check ($ "abc" :find (char=? _ #\z)) => (none))
(check ($ "测试" :find (char=? _ #\试)) => (option #\试))
(check ($ "" :find (lambda (c) #t)) => (none))

(check-report)
