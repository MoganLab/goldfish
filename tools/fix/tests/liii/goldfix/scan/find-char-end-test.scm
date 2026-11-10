;; find-char-end
;; 查找字符字面量 #\x 的结束位置：字符本身之后若紧跟分隔符则立即结束，
;; 否则持续扫描到下一个分隔符；未闭合时返回字符串长度。
;;
;; 语法
;; ----
;; (find-char-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 起始 #\ 的位置
;;
;; 返回值
;; ------
;; integer? 字符字面量的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：字符后紧跟空格
(check (find-char-end "#\\a b" 0) => 3)

;; 测试：字符后紧跟分号
(check (find-char-end "#\\a;" 0) => 3)

;; 测试：分隔符字符本身
(check (find-char-end "#\\;x" 0) => 3)

;; 测试：字面量不完整时返回字符串长度
(check (find-char-end "#\\" 0) => 2)

(check-report)
