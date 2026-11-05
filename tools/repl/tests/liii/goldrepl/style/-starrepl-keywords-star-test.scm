;; *repl-keywords*
;; 需要按关键字着色的 Scheme 关键字列表。
;;
;; 语法
;; ----
;; *repl-keywords*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; list
;; 关键字名称的字符串列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl style)
) ;import

(check-set-mode! 'report-failed)

;; 关键字表是一个列表
(check (list? *repl-keywords*) => #t)

;; 每个关键字都是字符串
(check-true (every (lambda (kw) (string? kw)) *repl-keywords*))

;; 常见关键字都在表中
(check-true (if (member "define" *repl-keywords*) #t #f))
(check-true (if (member "lambda" *repl-keywords*) #t #f))
(check-true (if (member "let" *repl-keywords*) #t #f))

(check-report)
