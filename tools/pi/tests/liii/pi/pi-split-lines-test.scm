(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-split-lines
;; 按换行符切分字符串，若以换行结尾则忽略末尾空段。

(check (pi-split-lines "") => '())
(check (pi-split-lines "\n") => '(""))
(check (pi-split-lines "line1\n") => '("line1"))
(check (pi-split-lines "line1\nline2\n") => '("line1" "line2"))
(check (pi-split-lines "line1\nline2") => '("line1" "line2"))
(check (pi-split-lines "line1\nline2\n\n") => '("line1" "line2" ""))
(check (pi-split-lines "line1\r\nline2\r\n") => '("line1\r" "line2\r"))

(check-report)
