;; string%drop-while
;; 从头跳过字符串中连续满足谓词的字符（对标 Scala String.dropWhile）。
;;
;; 语法
;; ----
;; ($ s :drop-while p)
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
;; string?
;; 去掉最长满足前缀后的剩余；全部满足返回 ""。UTF-8 语义。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "abc" :drop-while (char<? _ #\b)) => "bc")
(check ($ "abc" :drop-while (char<? _ #\z)) => "")
(check ($ "" :drop-while (lambda (c) #t)) => "")

(check-report)
