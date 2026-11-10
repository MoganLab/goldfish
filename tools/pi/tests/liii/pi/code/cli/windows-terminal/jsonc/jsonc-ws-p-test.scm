;; jsonc-ws?
;; 判断字符是否属于 JSONC 扫描使用的空白字符。
;;
;; 语法
;; ----
;; (jsonc-ws? c) -> boolean
;;
;; 参数
;; ----
;; c : char
;; 待判断的字符。
;;
;; 返回值
;; ----
;; boolean
;; 空格、制表符、换行或回车返回 #t，其余返回 #f。
;;

(import (scheme base) (liii check) (liii pi code cli windows-terminal jsonc))

(check-set-mode! 'report-failed)

(check-true (jsonc-ws? #\space))
(check-true (jsonc-ws? #\tab))
(check-true (jsonc-ws? #\newline))
(check-true (jsonc-ws? #\return))
(check-false (jsonc-ws? #\a))

(check-report)
