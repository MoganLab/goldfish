(import (liii check)
        (liii pi ai models list))

(check-set-mode! 'report-failed)

;; pi-format-token-count
;; 与 pi formatTokenCount() 逐条等价（整除无小数位，不整除保留1位小数，四舍五入远离零）

(check (pi-format-token-count 1000000) => "1M")
(check (pi-format-token-count 1048576) => "1.0M")
(check (pi-format-token-count 1250000) => "1.3M")
(check (pi-format-token-count 131072) => "131.1K")
(check (pi-format-token-count 64000) => "64K")
(check (pi-format-token-count 65536) => "65.5K")
(check (pi-format-token-count 128000) => "128K")
(check (pi-format-token-count 32768) => "32.8K")
(check (pi-format-token-count 8192) => "8.2K")
(check (pi-format-token-count 4096) => "4.1K")
(check (pi-format-token-count 512000) => "512K")
(check (pi-format-token-count 524288) => "524.3K")
(check (pi-format-token-count 204800) => "204.8K")
(check (pi-format-token-count 262144) => "262.1K")
(check (pi-format-token-count 500) => "500")
(check (pi-format-token-count 0) => "0")

(check-report)
