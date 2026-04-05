(import (liii check)
        (liii uri-predicate)
) ;import

(check-set-mode! 'report-failed)

;; uri-default-port
;; 获取 scheme 的默认端口。
;;
;; 语法
;; ----
;; (uri-default-port scheme)
;;
;; 返回值
;; ----
;; number? 或 #f
;;   返回 scheme 的默认端口号，未知 scheme 返回 #f。

;; 标准 scheme
(check (uri-default-port "http") => 80)
(check (uri-default-port "https") => 443)
(check (uri-default-port "ftp") => 21)
(check (uri-default-port "ssh") => 22)
(check (uri-default-port "smtp") => 25)
(check (uri-default-port "dns") => 53)
(check (uri-default-port "pop3") => 110)
(check (uri-default-port "imap") => 143)
(check (uri-default-port "ldap") => 389)

;; 未知 scheme
(check (uri-default-port "unknown") => #f)
(check (uri-default-port "xyz") => #f)

;; 大小写敏感（按标准应是小写）
(check (uri-default-port "HTTP") => #f)
(check (uri-default-port "HTTPS") => #f)

(check-report)
