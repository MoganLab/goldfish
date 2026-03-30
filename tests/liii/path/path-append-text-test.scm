(import (liii check)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; path-append-text
;; 追加文本到文件。
;;
;; 语法
;; ----
;; (path-append-text path content) → unspecified
;;
;; 参数
;; ----
;; path : string | path-value
;; 要追加的文件路径。
;; content : string
;; 要追加的文本内容。
;;
;; 返回值
;; -----
;; unspecified

;; 追加文本测试
(let ((append-file (path-join (path-temp-dir) "path-append-text.txt")))
  (when (path-exists? append-file)
    (delete-file (path->string append-file))
  ) ;when
  (path-write-text append-file "Initial content\n")
  (check (path-read-text append-file) => "Initial content\n")
  (path-append-text append-file "Appended content\n")
  (check (path-read-text append-file)
         => "Initial content\nAppended content\n"
  ) ;check
  (delete-file (path->string append-file))
) ;let

;; 追加到不存在文件测试
(let ((append-missing-file (path-join (path-temp-dir) "path-append-missing.txt")))
  (when (path-exists? append-missing-file)
    (delete-file (path->string append-missing-file))
  ) ;when
  (path-append-text append-missing-file "new")
  (check (path-read-text append-missing-file) => "new")
  (delete-file (path->string append-missing-file))
) ;let

(check-report)
