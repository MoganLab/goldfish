;; %repl-invalidate-symbol-cache!
;; 将符号缓存标记为失效，等待下次访问时惰性重建。
;;
;; 语法
;; ----
;; (%repl-invalidate-symbol-cache!)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过标记缓存失效生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
) ;import

(check-set-mode! 'report-failed)

;; 先建立符号缓存
(%repl-update-symbol-cache!)
(check (%repl-symbol-name? "*goldrepl-complete-invalidate-var*") => #f)

;; 新增根环境符号之后，缓存中还没有它
(eval '(define *goldrepl-complete-invalidate-var* 1) (rootlet))
(check (%repl-symbol-name? "*goldrepl-complete-invalidate-var*") => #f)

;; 标记缓存失效之后，查询候选会触发缓存重建
(%repl-invalidate-symbol-cache!)
(check-true (pair? (%repl-completions "*goldrepl-complete-invalidate-var*")))

;; 缓存重建之后新符号可以被检索到
(check (%repl-symbol-name? "*goldrepl-complete-invalidate-var*") => #t)

(check-report)
