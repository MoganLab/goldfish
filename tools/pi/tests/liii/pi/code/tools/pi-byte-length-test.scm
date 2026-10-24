(import (liii check) (liii pi code tools truncate))

(check-set-mode! 'report-failed)

;; pi-byte-length
;; 计算字符串的 UTF-8 字节长度。
(check (pi-byte-length "hello") => 5)
(check (pi-byte-length "金鱼") => 6)

(check-report)
