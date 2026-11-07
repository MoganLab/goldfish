;; pi-split-lines
;; 按换行符切分字符串，若以换行结尾则忽略末尾空段。

;; 语法
;; ----
;; (pi-split-lines s)

;; 参数
;; ----
;; s : string?，待切分文本

;; 返回值
;; ------
;; list of string，行列表

(import (liii check) (liii pi code core tools truncate))

(check-set-mode! 'report-failed)

(check (pi-split-lines "") => '())
(check (pi-split-lines "\n") => '(""))
(check (pi-split-lines "line1\n") => '("line1"))
(check (pi-split-lines "line1\nline2\n") => '("line1" "line2"))
(check (pi-split-lines "line1\nline2") => '("line1" "line2"))
(check (pi-split-lines "line1\nline2\n\n") => '("line1" "line2" ""))
(check (pi-split-lines "line1\r\nline2\r\n") => '("line1\r" "line2\r"))

(check-report)
