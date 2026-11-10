;; %repl-format-completion-menu
;; 排版补全菜单，最多显示 9 个候选。
;;
;; 语法
;; ----
;; (%repl-format-completion-menu cands . opt-w)
;;
;; 参数
;; ----
;; cands : list
;; 补全候选列表，元素形如 `(候选名 . 文档)`。
;; opt-w : integer
;; 可选的菜单宽度，默认 70。
;;
;; 返回值
;; ----
;; list
;; 菜单文本的行列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

;; 菜单中包含候选编号与候选名
(let* ((cands '(("string" . "doc1") ("string?" . "doc2") ("string=?" . "doc3")))
       (lines (%repl-format-completion-menu cands))
      ) ;
  (check-true (pair? lines))
  (check (length lines) => 3)
  (check-true (string-contains (car lines) "1"))
  (check-true (string-contains (car lines) "string"))
  (check-true (string-contains (car lines) "doc1"))
  (check-true (string-contains (cadr lines) "2"))
) ;let*

;; 候选过多时只显示前 9 个，并提示剩余数量
(let* ((cands
         (map
           (lambda (i) (cons (list->string (list (integer->char (+ 65 i)))) ""))
           (iota 12)
         ) ;map
       ) ;cands
       (lines (%repl-format-completion-menu cands))
      ) ;
  (check (length lines) => 10)
  (check-true (string-contains (car (reverse lines)) "more completions"))
) ;let*

;; 没有候选时菜单为空
(check (%repl-format-completion-menu '()) => '())

(check-report)
