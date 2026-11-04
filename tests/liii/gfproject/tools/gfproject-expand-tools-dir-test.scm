;; gfproject-expand-tools-dir
;; 展开用户波浪号或相对路径的工具目录。
;;
;; 语法
;; ----
;; (gfproject-expand-tools-dir dir) -> string
;;
;; 参数
;; ----
;; dir : string
;; 原始目录字符串。
;;
;; 返回值
;; ----
;; string
;; 展开波浪号或结合项目根目录后的绝对路径字符串。

(import (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

(let ((home (get-environment-variable "HOME")))
  (when (and home (> (string-length home) 0))
    (check (gfproject-expand-tools-dir "~") => home)
    (check (gfproject-expand-tools-dir "~/foo")
      => (path->string (path-join home "foo"))
    ) ;check
  ) ;when
) ;let

(when (not (os-windows?))
  (check (gfproject-expand-tools-dir "/abs/path") => "/abs/path")
) ;when

(let ((rel (gfproject-expand-tools-dir "my_tools")))
  (check (string? rel) => #t)
) ;let

(check-report)
