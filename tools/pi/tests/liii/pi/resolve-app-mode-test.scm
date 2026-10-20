(import (liii check)
  (liii pi)
  (liii pi args)
) ;import

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

;; 配合 parse-pi-args 解析与模式判定
;; 1. 缺省无参时 parse-pi-args 的结果与 resolve-app-mode 联动
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'mode) => #f)
  (check (pi-args-ref res 'print) => #f)
  (check (pi-args-ref res 'messages) => '())
  ;; 当未显式指定 --print、--mode 且标准输入/输出为 TTY 时，resolve-app-mode 解析为 'tui
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #t
           #t
         ) ;resolve-app-mode
    => 'tui
  ) ;check
  ;; 非 TTY 时（如管道），解析为 'print
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #f
           #t
         ) ;resolve-app-mode
    => 'print
  ) ;check
) ;let

;; 2. 显式指定 --mode tui
(let ((res (parse-pi-args '("--mode" "tui"))))
  (check (pi-args-ref res 'mode) => "tui")
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #t
           #t
         ) ;resolve-app-mode
    => 'tui
  ) ;check
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #f
           #f
         ) ;resolve-app-mode
    => 'tui
  ) ;check
) ;let

;; 3. 显式指定 --print 时优先为 'print
(let ((res (parse-pi-args '("-p"))))
  (check (pi-args-ref res 'print) => #t)
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #t
           #t
         ) ;resolve-app-mode
    => 'print
  ) ;check
) ;let

;; 4. 显式指定 --mode json 时为 'json
(let ((res (parse-pi-args '("--mode" "json"))))
  (check (pi-args-ref res 'mode) => "json")
  (check (resolve-app-mode (pi-args-ref res 'mode)
           (pi-args-ref res 'print)
           #t
           #t
         ) ;resolve-app-mode
    => 'json
  ) ;check
) ;let

(check-report)
