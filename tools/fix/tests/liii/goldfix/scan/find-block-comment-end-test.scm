;; find-block-comment-end
;; 查找块注释 #|...|# 的结束位置（不含结尾 |#），支持嵌套；
;; 未闭合时返回字符串长度。
;;
;; 语法
;; ----
;; (find-block-comment-end source start)
;;
;; 参数
;; ----
;; source : string? 源码文本
;; start : integer? 起始 #| 的位置
;;
;; 返回值
;; ------
;; integer? 块注释的结束位置

(import (liii check) (liii base) (liii goldfix scan))

(check-set-mode! 'report-failed)

;; 测试：普通块注释
(check (find-block-comment-end "#|a|#" 0) => 5)

;; 测试：嵌套块注释
(check (find-block-comment-end "#|#|a|#|#" 0) => 9)

;; 测试：未闭合时返回字符串长度
(check (find-block-comment-end "#|abc" 0) => 5)

(check-report)
