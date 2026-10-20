(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; resolve-app-mode
;; 根据命令行参数、stdin 是否为终端、stdout 是否为终端解析运行模式。
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'print)
(check (resolve-app-mode #f #f #f #t) => 'print)
(check (resolve-app-mode #f #f #t #f) => 'print)
;; 当 stdin 与 stdout 均为终端且未指定 print/json 时，解析为 'tui 交互模式
(check (resolve-app-mode #f #f #t #t) => 'tui)
(check (resolve-app-mode "tui" #f #f #f) => 'tui)
(check (resolve-app-mode 'tui #f #t #t) => 'tui)

(check-report)
