;; %repl-completions
;; 按前缀筛选符号缓存中的补全候选。
;;
;; 语法
;; ----
;; (%repl-completions prefix)
;;
;; 参数
;; ----
;; prefix : string
;; 补全前缀，候选符号名需要以该前缀开头。
;;
;; 返回值
;; ----
;; list
;; 由 (符号名 . 文档) 组成的候选列表，按符号缓存中的顺序排列。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
) ;import

(check-set-mode! 'report-failed)

(%repl-update-symbol-cache!)

;; 前缀命中候选
(let ((cands (%repl-completions "string-ap")))
  (check-true (pair? cands))
  (check-true
    (pair?
      (filter (lambda (e) (string=? (car e) "string-append")) cands)
    ) ;pair?
  ) ;check-true
) ;let

;; 所有候选都以给定前缀开头
(let ((cands (%repl-completions "map")))
  (check-true (pair? cands))
  (check-true
    (every (lambda (e) (string-starts? (car e) "map")) cands)
  ) ;check-true
) ;let

;; 没有匹配候选时返回空列表
(check (%repl-completions "no-such-symbol-xyz-abc") => '())

(check-report)
