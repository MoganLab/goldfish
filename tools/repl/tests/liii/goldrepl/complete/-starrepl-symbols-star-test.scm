;; *repl-symbols*
;; 全局符号缓存，保存 (符号名 . 文档) 形式的条目列表。
;;
;; 语法
;; ----
;; *repl-symbols*
;;
;; 参数
;; ----
;; 无
;; 该变量不接受参数。
;;
;; 返回值
;; ----
;; list
;; 缓存中的符号条目列表，元素形如 ("string-append" . "文档字符串")。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
) ;import

(check-set-mode! 'report-failed)

;; 缓存是一个列表
(check (list? *repl-symbols*) => #t)

;; 缓存条目都是 (字符串 . 字符串) 形式
(check-true
  (every
    (lambda (entry) (and (pair? entry) (string? (car entry)) (string? (cdr entry))))
    *repl-symbols*
  ) ;every
) ;check-true

;; 缓存内容可以通过补全接口观察到
(%repl-update-symbol-cache!)
(check (%repl-symbol-name? "string-append") => #t)

(check-report)
