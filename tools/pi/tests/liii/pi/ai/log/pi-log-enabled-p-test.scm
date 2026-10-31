(import (liii check) (liii os) (liii pi ai log))

(check-set-mode! 'report-failed)

;; pi-log-enabled?
;; 判断文件日志是否开启。
;;
;; 语法
;; ----
;; (pi-log-enabled?)
;;
;; 返回值
;; ----
;; boolean
;; 默认 #t；环境变量 PI_LOG 为 off、0、false、no（大小写与首尾空白不敏感）时返回 #f。
;;
;; 注意
;; ----
;; 关闭后 pi-log-error! 与 pi-log-http-error! 都不写盘，也不影响请求本身。

;; 1. 未设置环境变量：开启
(unsetenv "PI_LOG")
(check (pi-log-enabled?) => #t)

;; 2. 关闭取值
(for-each (lambda (value) (putenv "PI_LOG" value) (check (pi-log-enabled?) => #f))
  '("off" "0" "false" "no" "OFF" "Off")
) ;for-each

;; 3. 其它取值：开启
(for-each (lambda (value) (putenv "PI_LOG" value) (check (pi-log-enabled?) => #t))
  '("on" "1" "true" "yes" "debug")
) ;for-each

;; 4. 首尾空白不敏感
(putenv "PI_LOG" "  off  ")
(check (pi-log-enabled?) => #f)

(unsetenv "PI_LOG")

(check-report)
