;; ascii-upper-case?
;; 判断是否为 ASCII 大写字母。
;;
;; 语法
;; ----
;; (ascii-upper-case? x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要判断的字符或码点。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 大写字母则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖 A-Z 的字母边界。
;;
;; 错误处理
;; ----
;; 类型或范围不匹配时返回 #f

(import (liii check) (liii ascii))




(check-true (ascii-upper-case? #\A))
(check-false (ascii-upper-case? #\a))


(check-report)
