;; markdown-format-file-quiet
;; 安静格式化一个 Markdown 文件，返回状态与错误信息。
;;
;; 语法
;; ----
;; (markdown-format-file-quiet path-str)
;;
;; 参数
;; ----
;; path-str : string? Markdown 文件路径。
;;
;; 返回值
;; ------
;; list，形如 (status message)，status 为 updated、unchanged、cached 或 failed。

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
      (string-append "goldfmt-markdown-" (number->string (getpid)) ".md")
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () #t)
  (lambda ()
    (path-write-text (path file) "| A | B |\n|---|---|\n| x | y |\n")
    (check (car (markdown-format-file-quiet file)) => 'updated)
    (check (path-read-text (path file))
      =>
      "| A   | B   |\n| --- | --- |\n| x   | y   |\n"
    ) ;check
    (check-true
      (not (not (member (car (markdown-format-file-quiet file)) '(cached unchanged))))
    ) ;check-true
  ) ;lambda
  (lambda () (when (file-exists? file) (delete-file file)))
) ;dynamic-wind

(check-report)
