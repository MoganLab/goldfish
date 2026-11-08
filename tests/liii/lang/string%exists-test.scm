;; string%exists
;; 判断字符串中是否存在满足谓词的字符（对标 Scala String.exists）。
;;
;; 语法
;; ----
;; ($ s :exists p)
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
;; boolean
;; 任一字符满足 (p c) 为真则 #t；空串为 #f。UTF-8 语义。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "abc" :exists (lambda (c) (char=? c #\b))) => #t)
(check ($ "abc" :exists (lambda (c) (char=? c #\z))) => #f)
(check ($ "" :exists (lambda (c) #t)) => #f)
(check ($ "测试" :exists (lambda (c) (char=? c #\测))) => #t)

(check-report)
