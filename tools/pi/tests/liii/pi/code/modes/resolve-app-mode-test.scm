(import (liii check) (liii pi code modes))

(check-set-mode! 'report-failed)

;; resolve-app-mode
;; 根据命令行参数、stdin 是否为终端、stdout 是否为终端解析运行模式。
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
(check (resolve-app-mode #f #f #t #t) => 'interactive)

(check-report)
