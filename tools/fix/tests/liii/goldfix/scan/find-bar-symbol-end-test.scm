;; find-bar-symbol-end
;; 查找竖线符号 |...| 的结束位置（不含结尾竖线），
;; 支持反斜杠转义；未闭合时返回字符串长度。
;;
;; 语法
;; ----
;; (find-bar-symbol-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 起始竖线的位置
;;
;; 返回值
;; ------
;; integer? 竖线符号的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：普通竖线符号
(check (find-bar-symbol-end "|abc|" 0) => 5)

;; 测试：转义竖线不作为结束位置
(check (find-bar-symbol-end "|a\\|b|" 0) => 6)

;; 测试：未闭合时返回字符串长度
(check (find-bar-symbol-end "|abc" 0) => 4)

(check-report)
