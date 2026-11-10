;; reader-by-first-rest-child-indent
;; 按首个剩余子节点计算续行缩进。
;;
;; 语法
;; ----
;; (reader-by-first-rest-child-indent parent-indent rest)
;;
;; 参数
;; ----
;; parent-indent : integer?
;; 父节点的缩进列。
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

(check (reader-by-first-rest-child-indent 0 '((f) x)) => 2)
(check (reader-by-first-rest-child-indent 0 '(atom x)) => 2)

(check-report)
