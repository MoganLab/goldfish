(import (liii check)
        (liii string-cursor))

(check-set-mode! 'report-failed)

;; Out-of-range cursor regression tests (c42, c43, c44, c45)
(check-catch 'out-of-range (string-cursor-prev "a" -2000000000))
(check-catch 'out-of-range (string-cursor-back "a" -2000000000 1))
(check-catch 'out-of-range (string-cursor-diff "a" -2 -2000000002))
(check-catch 'out-of-range (string-cursor-forward "a" -2000000000 -1))

(check-report)
