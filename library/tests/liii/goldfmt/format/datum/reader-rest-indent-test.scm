;; reader-rest-indent
;; 按排版规则计算续行缩进。
;;
;; 语法
;; ----
;; (reader-rest-indent head-name parent-indent selected rest)
;;
;; 参数
;; ----
;; head-name : string?
;; 表单头名称。
;;
;; parent-indent : integer?
;; 父节点的缩进列。
;;
;; selected : list?
;; 首行选中项列表。
;;
;; rest : list?
;; 剩余的子节点列表。
;;
;; 返回值
;; ----
;; integer?
;; 续行的缩进列。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-rest-indent "lambda" 0 '() '()) => 2)

(check-report)
