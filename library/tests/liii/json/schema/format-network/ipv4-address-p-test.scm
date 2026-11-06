;; ipv4-address?
;; 判断字符串是否为点分十进制 IPv4 地址。
;;
;; 语法
;; ----
;; (ipv4-address? value)
;;
;; 参数
;; ----
;; value : string?
;; 待判断的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 恰好由 4 段十进制数组成且每段不超过 255 时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 仅接受纯 ASCII；段允许前导零（如 "01"），
;; 但空段、超过 4 段或段值大于 255 均判为无效。

(import (liii check) (liii json schema format-network))

(check-set-mode! 'report-failed)

(check (ipv4-address? "192.168.0.1") => #t)
(check (ipv4-address? "0.0.0.0") => #t)
(check (ipv4-address? "255.255.255.255") => #t)
(check (ipv4-address? "01.2.3.4") => #t)

;; 段值越界、段数不对、含非数字
(check (ipv4-address? "256.1.1.1") => #f)
(check (ipv4-address? "1.2.3") => #f)
(check (ipv4-address? "1.2.3.4.5") => #f)
(check (ipv4-address? "1.2.3.a") => #f)
(check (ipv4-address? "1.2.3.") => #f)
(check (ipv4-address? "") => #f)

(check-report)
