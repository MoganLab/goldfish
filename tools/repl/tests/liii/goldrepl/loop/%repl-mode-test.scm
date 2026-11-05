;; %repl-mode
;; 从命令行参数中解析 REPL 模式，缺省为 liii。
;;
;; 语法
;; ----
;; (%repl-mode)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; string
;; REPL 模式名称。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii list)
  (liii string)
  (liii goldrepl loop)
) ;import

(check-set-mode! 'report-failed)

(let* ((args (command-line)) (mode (%repl-mode)))
  ;; 返回的是非空字符串
  (check (string? mode) => #t)
  (check (> (string-length mode) 0) => #t)
  ;; 模式要么是缺省值，要么来自命令行参数
  (check
    (or (string=? mode "liii") (any (lambda (arg) (string-contains arg mode)) args))
    =>
    #t
  ) ;check
) ;let*

(check-report)
