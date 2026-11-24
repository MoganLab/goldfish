;; markdown-fmt-one
;; Worker Pool 使用的单文件 Markdown 格式化任务。
;;
;; 语法
;; ----
;; (markdown-fmt-one path)
;;
;; 参数
;; ----
;; path : string? Markdown 文件路径。
;;
;; 返回值
;; ------
;; list，包含文件路径、处理状态和错误信息。

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
      (string-append "goldfmt-markdown-one-" (number->string (getpid)) ".md")
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () #t)
  (lambda ()
    (path-write-text (path file) "| A | B |\n|---|---|\n| x | y |\n")
    (let ((result (markdown-fmt-one file)))
      (check (car result) => file)
      (check (cadr result) => 'updated)
    ) ;let
  ) ;lambda
  (lambda () (when (file-exists? file) (delete-file file)))
) ;dynamic-wind

(check-report)
