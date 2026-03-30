(import (liii check)
        (liii ascii))

;; ascii-string-ci=?
;; 按 ASCII 大小写无关规则比较两个字符串是否相等。
;;
;; 语法
;; ----
;; (ascii-string-ci=? string1 string2)
;;
;; 参数
;; ----
;; string1, string2 : string?
;; 要比较的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 若大小写折叠后字符串相等则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 仅针对 ASCII 字符串比较。
;;
;; 示例
;; ----
;; (ascii-string-ci=? "GoldFish" "goldfish") => #t
;;
;; 错误处理
;; ----
;; 参数类型不匹配时按过程约定报错

(check-true (ascii-string-ci=? "GoldFish" "goldfish"))
(check-false (ascii-string-ci=? "goldfish" "gold-fish"))

(check-report)
