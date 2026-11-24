;; markdown-check-file-quiet
;; 检查 Markdown 文件是否等于 cmark 往返转换后的内容，不写回文件。
;;
;; 语法
;; ----
;; (markdown-check-file-quiet path-str)
;;
;; 参数
;; ----
;; path-str : string? Markdown 文件路径。
;;
;; 返回值
;; ------
;; boolean?，已格式化返回 #t。

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
      (string-append "goldfmt-markdown-check-" (number->string (getpid)) ".md")
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () #t)
  (lambda ()
    (path-write-text (path file) "| A | B |\n|---|---|\n| x | y |\n")
    (check-false (markdown-check-file-quiet file))
    (path-write-text (path file)
      (format-markdown-string (path-read-text (path file)))
    ) ;path-write-text
    (check-true (markdown-check-file-quiet file))
  ) ;lambda
  (lambda () (when (file-exists? file) (delete-file file)))
) ;dynamic-wind

(check-report)
