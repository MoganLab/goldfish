;; %repl-symbols-list
;; 返回当前的符号缓存列表，缓存失效时先重建。
;;
;; 语法
;; ----
;; (%repl-symbols-list)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
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

;; 缓存失效时会自动重建，因此可以直接取到符号
(check-true (pair? (%repl-symbols-list)))
(check (%repl-symbol-name? "string-append") => #t)

;; 条目都是 (字符串 . 字符串) 形式
(check-true
  (every
    (lambda (entry) (and (pair? entry) (string? (car entry)) (string? (cdr entry))))
    (%repl-symbols-list)
  ) ;every
) ;check-true

;; 缓存失效之后，下次取用符号列表会重建缓存
(eval '(define *goldrepl-complete-symbols-list-var* 1) (rootlet))
(%repl-invalidate-symbol-cache!)
(check-true (pair? (%repl-symbols-list)))
(check (%repl-symbol-name? "*goldrepl-complete-symbols-list-var*") => #t)

(check-report)
