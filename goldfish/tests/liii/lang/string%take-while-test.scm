;; string%take-while
;; 从头取字符串中连续满足谓词的字符（对标 Scala String.takeWhile）。
;;
;; 语法
;; ----
;; ($ s :take-while p)
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
;; 最长满足前缀；首字符不满足或空串返回 ""。UTF-8 语义。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "abc" :take-while (char<? _ #\c)) => "ab")
(check ($ "cba" :take-while (char<? _ #\c)) => "")
(check ($ "" :take-while (lambda (c) #t)) => "")
(check
  ($ "测试法" :take-while (lambda (c) (not (char=? c #\法))))
  =>
  "测试"
) ;check

(check-report)
