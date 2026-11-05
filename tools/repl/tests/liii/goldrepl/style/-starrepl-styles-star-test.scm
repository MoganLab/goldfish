;; *repl-styles*
;; 样式名到 ANSI 控制序列的映射表。
;;
;; 语法
;; ----
;; *repl-styles*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; list
;; 由 (样式名 . ANSI 控制序列) 组成的列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl style)
) ;import

(check-set-mode! 'report-failed)

;; 样式表是一个列表
(check (list? *repl-styles*) => #t)

;; 每个条目都是 (字符串 . 字符串)
(check-true
  (every
    (lambda (entry) (and (pair? entry) (string? (car entry)) (string? (cdr entry))))
    *repl-styles*
  ) ;every
) ;check-true

;; 常用样式都在样式表中
(check-true (if (assoc "keyword" *repl-styles*) #t #f))
(check-true (if (assoc "error" *repl-styles*) #t #f))
(check-true (if (assoc "reset" *repl-styles*) #t #f))

(check-report)
