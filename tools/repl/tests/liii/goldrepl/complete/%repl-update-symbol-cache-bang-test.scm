;; %repl-update-symbol-cache!
;; 扫描根环境，重建全局符号缓存。
;;
;; 语法
;; ----
;; (%repl-update-symbol-cache!)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过重建符号缓存生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl complete)
) ;import

(check-set-mode! 'report-failed)

;; 更新之前缓存中没有任何符号
(check (%repl-symbol-name? "string-append") => #f)

;; 更新之后可以检索到标准库符号
(%repl-update-symbol-cache!)
(check (%repl-symbol-name? "string-append") => #t)

;; 缓存中包含 REPL 自身导出的符号
(check (%repl-symbol-name? "%repl-completions") => #t)

;; 重复更新是幂等的
(%repl-update-symbol-cache!)
(check (%repl-symbol-name? "string-append") => #t)

(check-report)
