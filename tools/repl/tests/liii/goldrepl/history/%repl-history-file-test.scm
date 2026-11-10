;; %repl-history-file
;; 返回历史文件的完整路径。
;;
;; 语法
;; ----
;; (%repl-history-file)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; string
;; 历史文件的完整路径。

(import (scheme base)
  (liii check)
  (liii os)
  (liii string)
  (liii goldrepl history)
) ;import

(check-set-mode! 'report-failed)

;; 返回的是字符串路径
(check (string? (%repl-history-file)) => #t)

;; 历史文件位于历史目录中，文件名为 history
(check (string-ends? (%repl-history-file) "/history") => #t)
(check (string-starts? (%repl-history-file) (%repl-history-dir)) => #t)

(check-report)
