;; parse-windows-path
;; 将 Windows 路径字符串解析为段向量、盘符与根。
;;
;; 语法
;; ----
;; (parse-windows-path s)
;;
;; 参数
;; ----
;; s : string?
;; Windows 路径字符串，同时识别 \ 与 / 分隔符。
;;
;; 返回值
;; ----
;; values
;; 三个值：parts 段向量、drive 字符串与 root 字符（或 #f）。
;;
;; 描述
;; ----
;; 涵盖 UNC、盘符绝对/相对、当前盘根与普通相对路径五种形式。

(import (liii check) (liii path parse) (srfi srfi-11))

(check-set-mode! 'report-failed)

;; UNC：\\server\share\a\b
(let-values (((parts drive root) (parse-windows-path "\\\\server\\share\\a\\b")))
  (check parts => #("a" "b"))
  (check drive => "\\\\server\\share")
  (check root => #\\)
) ;let-values

;; 盘符绝对路径：C:\a\b
(let-values (((parts drive root) (parse-windows-path "C:\\a\\b")))
  (check parts => #("a" "b"))
  (check drive => "C")
  (check root => #\\)
) ;let-values

;; 盘符相对路径：C:foo
(let-values (((parts drive root) (parse-windows-path "C:foo")))
  (check parts => #("foo"))
  (check drive => "C")
  (check root => #f)
) ;let-values

;; 当前盘根路径：\foo
(let-values (((parts drive root) (parse-windows-path "\\foo")))
  (check parts => #("foo"))
  (check drive => "")
  (check root => #\\)
) ;let-values

;; 相对路径：foo\bar
(let-values (((parts drive root) (parse-windows-path "foo\\bar")))
  (check parts => #("foo" "bar"))
  (check drive => "")
  (check root => #f)
) ;let-values

;; / 与 \ 混合也识别
(let-values (((parts drive root) (parse-windows-path "C:/a/b")))
  (check parts => #("a" "b"))
  (check drive => "C")
  (check root => #\\)
) ;let-values

(check-report)
