;; pi-format-token-count
;; 将 token 整数数量格式化为紧凑字符串（原样、k、M 缩写）
;;
;; 语法
;; ----
;; (pi-format-token-count count)
;;
;; 参数
;; ----
;; count : integer
;; token 数量。
;;
;; 返回值
;; ----
;; string
;; 紧凑格式化后的字符串。

;; 小于 1000：原数字

(import (scheme base)
  (liii check)
  (liii pi code core session-stats)
) ;import

(check-set-mode! 'report-failed)

(check (pi-format-token-count 0) => "0")
(check (pi-format-token-count 45) => "45")
(check (pi-format-token-count 999) => "999")

;; 1000 ~ 9999：保留一位小数并带 k 后缀
(check (pi-format-token-count 1000) => "1.0k")
(check (pi-format-token-count 1200) => "1.2k")
(check (pi-format-token-count 9540) => "9.5k")

;; 10000 ~ 999999：四舍五入整数并带 k 后缀
(check (pi-format-token-count 15000) => "15k")
(check (pi-format-token-count 15200) => "15k")
(check (pi-format-token-count 158455) => "158k")

;; 大于等于 1000000：保留一位小数并带 M 后缀
(check (pi-format-token-count 1000000) => "1.0M")
(check (pi-format-token-count 1300000) => "1.3M")
(check (pi-format-token-count 20000000) => "20.0M")

(check-report)
