;; join-lines
;; 用换行符连接行列表，得到多行文本。
;;
;; 语法
;; ----
;; (join-lines lines)
;;
;; 参数
;; ----
;; lines : list
;; 逻辑行字符串列表。
;;
;; 返回值
;; ----
;; string
;; 以 \n 连接后的多行文本。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? join-lines))

(check (join-lines '()) => "")
(check (join-lines '("单行")) => "单行")
(check (join-lines '("第一行" "第二行")) => "第一行\n第二行")
(check (join-lines '("a" "" "b")) => "a\n\nb")

(check-report)
