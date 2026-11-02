;; pi-slash-command-matches
;; 匹配输入行中的斜杠命令，返回候选命令列表。
;;
;; 语法
;; ----
;; (pi-slash-command-matches text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入的当前文本。
;;
;; 返回值
;; ----
;; list?
;; 匹配到的命令 alist 列表；若无匹配或已进入参数输入则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive slash-command-menu)
) ;import

(check-set-mode! 'report-failed)

;; 1. 前缀 / 匹配所有斜杠命令
(let ((matches (pi-slash-command-matches "/")))
  (check-true (pair? matches))
  (check (> (length matches) 0) => #t)
) ;let

;; 2. 部分匹配
(let ((matches (pi-slash-command-matches "/mod")))
  (check-true (pair? matches))
  (check (cdr (assoc 'name (car matches))) => "model")
) ;let

;; 3. 包含空格时不匹配
(check-false (pi-slash-command-matches "/model "))

;; 4. 非斜杠开头不匹配
(check-false (pi-slash-command-matches "model"))

(check-report)
