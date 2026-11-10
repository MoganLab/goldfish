;; string-prefix-at?
;; 判断字符串在指定位置是否具有给定前缀；越界时返回 #f。
;;
;; 语法
;; ----
;; (string-prefix-at? str prefix pos)
;;
;; 参数
;; ----
;; str : string? 待检查的字符串
;; prefix : string? 前缀
;; pos : integer? 起始位置
;;
;; 返回值
;; ------
;; boolean? 匹配时为 #t，否则为 #f

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：指定位置匹配前缀
(check (string-prefix-at? "hello" "ell" 1) => #t)

;; 测试：位置不匹配
(check (string-prefix-at? "hello" "ell" 0) => #f)

;; 测试：空前缀总是匹配
(check (string-prefix-at? "hello" "" 0) => #t)

;; 测试：前缀超出字符串长度
(check (string-prefix-at? "hi" "hello" 0) => #f)

(check-report)
