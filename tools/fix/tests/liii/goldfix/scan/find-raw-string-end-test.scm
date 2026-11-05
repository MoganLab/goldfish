;; find-raw-string-end
;; 查找原始字符串 #"..."# 的结束位置（不含结尾引号）。
;; 分隔符为空时以两个双引号结束；未闭合时返回字符串长度。
;;
;; 语法
;; ----
;; (find-raw-string-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 起始 #" 的位置
;;
;; 返回值
;; ------
;; integer? 原始字符串的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：带分隔符的原始字符串
(check (find-raw-string-end "#\"END\")END\"" 0) => 11)

;; 测试：空分隔符的原始字符串
(check (find-raw-string-end "#\"\"(not-code)\"\"" 0) => 15)

;; 测试：未闭合时返回字符串长度
(check (find-raw-string-end "#\"abc" 0) => 5)

(check-report)
