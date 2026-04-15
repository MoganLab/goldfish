(import (liii check)
  (liii uri-predicate)
) ;import


(check-set-mode! 'report-failed)


;; uri-network-scheme?
;; 检查是否为网络 scheme（有默认端口定义的 scheme）。
;;
;; 语法
;; ----
;; (uri-network-scheme? scheme)
;;
;; 返回值
;; ----
;; boolean?
;;   如果是网络 scheme 返回 #t，否则返回 #f。


;; 网络 scheme
(check (uri-network-scheme? "http")
  =>
  #t
) ;check
(check (uri-network-scheme? "https")
  =>
  #t
) ;check
(check (uri-network-scheme? "ftp")
  =>
  #t
) ;check
(check (uri-network-scheme? "ssh")
  =>
  #t
) ;check
(check (uri-network-scheme? "smtp")
  =>
  #t
) ;check
(check (uri-network-scheme? "dns")
  =>
  #t
) ;check
(check (uri-network-scheme? "pop3")
  =>
  #t
) ;check
(check (uri-network-scheme? "imap")
  =>
  #t
) ;check
(check (uri-network-scheme? "ldap")
  =>
  #t
) ;check


;; 非网络 scheme
(check (uri-network-scheme? "file")
  =>
  #f
) ;check
(check (uri-network-scheme? "mailto")
  =>
  #f
) ;check
(check (uri-network-scheme? "data")
  =>
  #f
) ;check
(check (uri-network-scheme? "javascript")
  =>
  #f
) ;check


;; 大小写敏感
(check (uri-network-scheme? "HTTP")
  =>
  #f
) ;check
(check (uri-network-scheme? "HTTPS")
  =>
  #f
) ;check


;; 空值和非法输入
(check (uri-network-scheme? "") => #f)
(check (uri-network-scheme? #f) => #f)


(check-report)
