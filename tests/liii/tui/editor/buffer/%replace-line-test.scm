;; %replace-line
;; 替换行列表中指定索引位置的行字符串。
;;
;; 语法
;; ----
;; (%replace-line lines row new-line)
;;
;; 参数
;; ----
;; lines : list?
;; 原行列表。
;;
;; row : integer?
;; 待替换的行号索引（0-based）。
;;
;; new-line : string?
;; 替换后的新行文本。
;;
;; 返回值
;; ----
;; list?
;; 替换完成的新行列表。

(import (scheme base) (liii check) (liii tui editor buffer))

(check-set-mode! 'report-failed)

(check (%replace-line '("a" "b" "c") 1 "X") => '("a" "X" "c"))
(check (%replace-line '("first") 0 "new") => '("new"))

(check-report)
