(import (liii check) (liii os) (liii pi ai log))

(check-set-mode! 'report-failed)

;; pi-log-enabled?
;;
;; 文件日志开关：默认开启；PI_LOG 为 off/0/false/no 时关闭。

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
