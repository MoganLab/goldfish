(import (liii check)
        (liii go))
(check-set-mode! 'report-failed)
;; value-error + 非 channel 的 irritant（go-spawn names/vals 长度不匹配）
(check-catch 'value-error (g_go-spawn '(a) '() '(begin)))
(check-report)
