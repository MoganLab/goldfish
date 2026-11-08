;; string%map
;; 对字符串每个字符应用变换函数，返回新字符串（对标 Scala String.map）。
;;
;; 语法
;; ----
;; ($ s :map f)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串（UTF-8）。
;;
;; f : procedure?
;; 单参字符变换函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; string?
;; 与 s 等字符数的新字符串。
;;
;; 描述
;; ----
;; string 路径基于 (liii string-cursor) 的 UTF-8 游标遍历，
;; 中文/emoji 等多字节字符按单字符正确处理。

(import (liii check) (liii lang) (scheme char))

(check-set-mode! 'report-failed)

(check ($ "héllo wörld" :map char-upcase) => "HÉLLO WÖRLD")
(check ($ "" :map char-upcase) => "")
(check ($ "测试" :map (lambda (c) c)) => "测试")
(check ($ "🎉🎊" :map (lambda (c) c)) => "🎉🎊")

(check-report)
