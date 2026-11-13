;; resolve-app-mode
;; 根据命令行参数与终端状态解析运行模式。
;;
;; 语法
;; ----
;; (resolve-app-mode mode-option print-flag stdin-tty stdout-tty)
;;
;; 参数
;; ----
;; mode-option : (or string symbol #f)
;; 命令行指定的运行模式。
;;
;; print-flag : boolean
;; 是否使用 print 模式。
;;
;; stdin-tty : boolean
;; 标准输入是否为终端。
;;
;; stdout-tty : boolean
;; 标准输出是否为终端。
;;
;; 返回值
;; ----
;; symbol
;; 解析出的运行模式。

(import (liii check) (liii pi code modes))

(check-set-mode! 'report-failed)

;; resolve-app-mode
;; 根据命令行参数、stdin 是否为终端、stdout 是否为终端解析运行模式。
(check (resolve-app-mode "json" #f #t #t) => 'json)
(check (resolve-app-mode 'json #f #t #t) => 'json)
(check (resolve-app-mode "verbose" #f #t #t) => 'verbose)
(check (resolve-app-mode 'verbose #f #t #t) => 'verbose)
(check (resolve-app-mode "text" #f #t #t) => 'print)
(check (resolve-app-mode 'text #f #t #t) => 'print)
(check (resolve-app-mode #f #t #t #t) => 'verbose)
(check (resolve-app-mode #f #f #f #t) => 'verbose)
(check (resolve-app-mode #f #f #t #f) => 'verbose)
(check (resolve-app-mode #f #f #t #t) => 'interactive)

(check-report)
