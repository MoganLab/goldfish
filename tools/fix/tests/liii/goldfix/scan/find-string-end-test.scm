;; find-string-end
;; 查找双引号字符串字面量的结束位置（不含结尾引号），
;; 支持反斜杠转义；未闭合时返回字符串长度。
;;
;; 语法
;; ----
;; (find-string-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 起始引号的位置
;;
;; 返回值
;; ------
;; integer? 字符串字面量的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：普通字符串字面量
(check (find-string-end "\"abc\"" 0) => 5)

;; 测试：转义引号不作为结束位置
(check (find-string-end "\"a\\\"b\"" 0) => 6)

;; 测试：未闭合时返回字符串长度
(check (find-string-end "\"abc" 0) => 4)

(check-report)
