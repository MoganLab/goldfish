(import (liii check)
  (liii path)
  (liii error)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-write-text
;; 写入文本文件内容。
;;
;; 语法
;; ----
;; (path-write-text path content) → unspecified
;;
;; 参数
;; ----
;; path : string | path-value
;; 要写入的文件路径。
;; content : string
;; 要写入的文本内容。
;;
;; 返回值
;; -----
;; unspecified
;;
;; 错误处理
;; ----
;; type-error 当 content 不是字符串时。

;; 基本写入测试
(let ((write-file (path-join (path-temp-dir)
                    "path-write-text-basic.txt"
                  ) ;path-join
      ) ;write-file
     ) ;
  (when (path-exists? write-file)
    (delete-file (path->string write-file))
  ) ;when
  (path-write-text write-file
    "test content"
  ) ;path-write-text
  (check (path-read-text write-file)
    =>
    "test content"
  ) ;check
  ;; 覆盖写入
  (path-write-text write-file
    "new content"
  ) ;path-write-text
  (check (path-read-text write-file)
    =>
    "new content"
  ) ;check
  (delete-file (path->string write-file))
) ;let

;; 写入空字符串
(let ((empty-file (path-join (path-temp-dir)
                    "path-write-text-empty.txt"
                  ) ;path-join
      ) ;empty-file
     ) ;
  (when (path-exists? empty-file)
    (delete-file (path->string empty-file))
  ) ;when
  (path-write-text empty-file "")
  (check (path-read-text empty-file)
    =>
    ""
  ) ;check
  (check (path-getsize empty-file) => 0)
  (delete-file (path->string empty-file))
) ;let

;; 多层目录写入测试
(let* ((base-dir (path-join (path-temp-dir)
                   "path-write-text-depth"
                 ) ;path-join
       ) ;base-dir
       (nested-dir (path-join base-dir "nested")
       ) ;nested-dir
       (deep-file (path-join nested-dir "deep.txt")
       ) ;deep-file
      ) ;
  (when (path-exists? deep-file)
    (delete-file (path->string deep-file))
  ) ;when
  (when (path-exists? nested-dir)
    (rmdir (path->string nested-dir))
  ) ;when
  (when (path-exists? base-dir)
    (rmdir (path->string base-dir))
  ) ;when
  (mkdir (path->string base-dir))
  (mkdir (path->string nested-dir))
  (path-write-text deep-file
    "Deeply nested file content"
  ) ;path-write-text
  (check (path-read-text deep-file)
    =>
    "Deeply nested file content"
  ) ;check
  (delete-file (path->string deep-file))
  (rmdir (path->string nested-dir))
  (rmdir (path->string base-dir))
) ;let*

;; 错误处理测试
(check-catch 'type-error
  (path-write-text (path-join (path-temp-dir) "test.txt")
    123
  ) ;path-write-text
) ;check-catch

(check-report)
