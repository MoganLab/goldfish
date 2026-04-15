(import (liii check) (liii path))

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
(let ((append-file (path-join (path-temp-dir)
                     "path-append-text.txt"
                   ) ;path-join
      ) ;append-file
     ) ;
  (when (path-exists? append-file)
    (delete-file (path->string append-file))
  ) ;when
  (path-write-text append-file
    "Initial content\n"
  ) ;path-write-text
  (check (path-read-text append-file)
    =>
    "Initial content\n"
  ) ;check
  (path-append-text append-file
    "Appended content\n"
  ) ;path-append-text
  (check (path-read-text append-file)
    =>
    "Initial content\nAppended content\n"
  ) ;check
  (delete-file (path->string append-file))
) ;let

;; 追加到不存在文件测试
(let ((append-missing-file (path-join (path-temp-dir)
                             "path-append-missing.txt"
                           ) ;path-join
      ) ;append-missing-file
     ) ;
  (when (path-exists? append-missing-file)
    (delete-file (path->string append-missing-file)
    ) ;delete-file
  ) ;when
  (path-append-text append-missing-file
    "new"
  ) ;path-append-text
  (check (path-read-text append-missing-file)
    =>
    "new"
  ) ;check
  (delete-file (path->string append-missing-file)
  ) ;delete-file
) ;let

(check-report)
