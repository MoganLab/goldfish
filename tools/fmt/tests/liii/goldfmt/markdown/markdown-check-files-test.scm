;; markdown-check-files
;; 批量检查 Markdown 文件，返回所有需要格式化的路径。
;;
;; 语法
;; ----
;; (markdown-check-files files cfg)
;;
;; 参数
;; ----
;; files : list of string，待检查文件路径。
;; cfg : JSON 对象，提供 Markdown exclude 配置。
;;
;; 返回值
;; ------
;; list of string，未格式化文件列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii json)
  (liii goldfmt markdown)
) ;import

(check-set-mode! 'report-failed)

(define file
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfmt-markdown-check-files-" (number->string (getpid)) ".md")
    ) ;path-join
  ) ;path->string
) ;define

(dynamic-wind (lambda () #t)
  (lambda ()
    (path-write-text (path file) "| A | B |\n|---|---|\n| x | y |\n")
    (check (markdown-check-files (list file) (string->json "{}")) => (list file))
    (path-write-text (path file)
      (format-markdown-string (path-read-text (path file)))
    ) ;path-write-text
    (check (markdown-check-files (list file) (string->json "{}")) => '())
  ) ;lambda
  (lambda () (when (file-exists? file) (delete-file file)))
) ;dynamic-wind

(check-report)
