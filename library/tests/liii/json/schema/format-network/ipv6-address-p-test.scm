;; ipv6-address?
;; 判断字符串是否为 IPv6 地址。
;;
;; 语法
;; ----
;; (ipv6-address? value)
;;
;; 参数
;; ----
;; value : string?
;; 待判断的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 合法的 IPv6 地址返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 支持完整 8 组形式与单个 "::" 缩写；每组为 1~4 位十六进制，
;; 大小写均可。含 "::" 时组数须少于 8，不含时须恰好 8 组。

(import (liii check) (liii json schema format-network))

(check-set-mode! 'report-failed)

(check (ipv6-address? "1:2:3:4:5:6:7:8") => #t)
(check (ipv6-address? "2001:0db8:85a3:0000:0000:8a2e:0370:7334") => #t)
(check (ipv6-address? "::") => #t)
(check (ipv6-address? "::1") => #t)
(check (ipv6-address? "2001:db8::1") => #t)
(check (ipv6-address? "2001:db8::") => #t)
(check (ipv6-address? "ABCD::1") => #t)

;; 非法：组数过多、十六进制越界、非冒号分隔
(check (ipv6-address? "1:2:3:4:5:6:7:8:9") => #f)
(check (ipv6-address? "gggg::1") => #f)
(check (ipv6-address? "12345::") => #f)
(check (ipv6-address? "1.2.3.4") => #f)
(check (ipv6-address? "") => #f)

(check-report)
