;; find-line-comment-end
;; 查找行注释的结束位置，即下一个换行符所在位置；
;; 没有换行符时返回字符串长度。
;;
;; 语法
;; ----
;; (find-line-comment-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 注释起始位置
;;
;; 返回值
;; ------
;; integer? 行注释的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：注释在换行符处结束
(check (find-line-comment-end ";abc\ndef" 0) => 4)

;; 测试：没有换行符时返回字符串长度
(check (find-line-comment-end ";abc" 0) => 4)

(check-report)
