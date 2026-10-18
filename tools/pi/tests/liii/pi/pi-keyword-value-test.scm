(import (liii check) (liii pi read))

(check-set-mode! 'report-failed)

;; pi-keyword-value
;; 从函数参数列表中按关键字读取对应的值。
(check (pi-keyword-value :timeout '(:timeout 30 :cwd "/tmp") 10) => 30)
(check (pi-keyword-value :timeout '(:cwd "/tmp") 10) => 10)

(check-report)
