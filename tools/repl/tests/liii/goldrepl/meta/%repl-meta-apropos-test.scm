;; %repl-meta-apropos
;; 按子串搜索符号缓存，返回描述行列表。
;;
;; 语法
;; ----
;; (%repl-meta-apropos arg)
;;
;; 参数
;; ----
;; arg : string
;; 待搜索的子串。
;;
;; 返回值
;; ----
;; list
;; 匹配符号的描述行列表；没有匹配时返回一行提示信息。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
  (liii goldrepl meta)
) ;import

(check-set-mode! 'report-failed)

;; 搜索前需要先建立符号缓存
(%repl-update-symbol-cache!)

;; 空参数返回用法提示
(let ((lines (%repl-meta-apropos "")))
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "Usage:"))
) ;let

;; 可以搜索到符号
(let ((lines (%repl-meta-apropos "string-app")))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "string-append")) lines))
) ;let

;; 没有匹配时返回提示信息
(let ((lines (%repl-meta-apropos "no-such-symbol-xyz")))
  (check-true (pair? lines))
  (check-true (string-contains (car lines) "No symbol matches"))
) ;let

(check-report)
