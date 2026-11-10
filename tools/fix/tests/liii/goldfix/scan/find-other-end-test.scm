;; find-other-end
;; 查找普通记号的结束位置：扫描到第一个分隔符为止，
;; 遇到块注释起始 #| 时立即结束；没有分隔符时返回字符串长度。
;;
;; 语法
;; ----
;; (find-other-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 记号起始位置
;;
;; 返回值
;; ------
;; integer? 普通记号的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：扫描到左括号处结束
(check (find-other-end "abc(def" 0) => 3)

;; 测试：块注释起始处立即结束
(check (find-other-end "#|abc" 0) => 0)

;; 测试：没有分隔符时返回字符串长度
(check (find-other-end "abc" 0) => 3)

(check-report)
