;; markdown-check-one
;; Worker Pool 使用的单文件 Markdown 检查任务。
;;
;; 语法
;; ----
;; (markdown-check-one path)
;;
;; 参数
;; ----
;; path : string? Markdown 文件路径。
;;
;; 返回值
;; ------
;; list，包含文件路径、是否已格式化和保留字段。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldfmt markdown)
) ;import

(check-set-mode! 'report-failed)

(define file
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfmt-markdown-check-one-" (number->string (getpid)) ".md")
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () #t)
  (lambda ()
    (path-write-text (path file) "| A | B |\n|---|---|\n| x | y |\n")
    (let ((result (markdown-check-one file)))
      (check (car result) => file)
      (check-false (cadr result))
    ) ;let
    (path-write-text (path file)
      (format-markdown-string (path-read-text (path file)))
    ) ;path-write-text
    (check-true (cadr (markdown-check-one file)))
  ) ;lambda
  (lambda () (when (file-exists? file) (delete-file file)))
) ;dynamic-wind

(check-report)
