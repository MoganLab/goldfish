;; parse-unc
;; 将 UNC 路径解析为段向量、盘符与根。
;;
;; 语法
;; ----
;; (parse-unc normalized len)
;;
;; 参数
;; ----
;; normalized : string?
;; 以 \\ 开头的 UNC 路径字符串。
;; len : integer?
;; 参与解析的字符串长度。
;;
;; 返回值
;; ----
;; values
;; 三个值：parts 段向量、drive（UNC 主机/共享名）与 root 字符。
;;
;; 描述
;; ----
;; 仅 \\server（无 share）时 drive 为 "\\\\server" 且 root 为 #f（对齐 pathlib）；
;; 含 share 时 drive 为 "\\\\server\\share" 且 root 为 #\\。

(import (liii check) (liii path parse) (srfi srfi-11))

(check-set-mode! 'report-failed)

;; 仅 \\server：root 为 #f
(let-values (((parts drive root) (parse-unc "\\\\server" 8)))
  (check parts => #())
  (check drive => "\\\\server")
  (check root => #f)
) ;let-values

;; \\server\share：root 为 #\\
(let-values (((parts drive root) (parse-unc "\\\\srv\\share" 11)))
  (check parts => #())
  (check drive => "\\\\srv\\share")
  (check root => #\\)
) ;let-values

;; \\server\share\path...
(let-values (((parts drive root) (parse-unc "\\\\srv\\share\\a\\b" 15)))
  (check parts => #("a" "b"))
  (check drive => "\\\\srv\\share")
  (check root => #\\)
) ;let-values

(check-report)
