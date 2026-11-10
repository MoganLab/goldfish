;; string%count
;; 统计字符串中满足谓词的字符个数（对标 Scala String.count）。
;;
;; 语法
;; ----
;; ($ s :count p)
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
;; integer?
;; 满足 (p c) 为真的字符个数。
;;
;; 描述
;; ----
;; UTF-8 语义，基于 (liii string-cursor) 的 string-count（参数序 (pred s)）。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "aXbXc" :count (lambda (c) (char=? c #\X))) => 2)
(check ($ "" :count (lambda (c) #t)) => 0)
(check ($ "测试123" :count char-numeric?) => 3)

(check-report)
