;; path-copy-into
;; 将文件复制到目标目录，保留原文件名。
;;
;; 语法
;; ----
;; (path-copy-into source target-dir)
;;
;; 参数
;; ----
;; source : path | string
;; 源文件路径。
;; target-dir : path | string
;; 目标目录路径。
;;
;; 返回值
;; ------
;; boolean
;; 复制成功返回 #t，失败返回 #f。

(import (liii check) (liii path) (liii os) (liii string))

(check-set-mode! 'report-failed)

;; 使用临时目录，避免依赖仓库布局与工作目录
(let* ((base
         (path-join (path-temp-dir)
           (string-append "gf-path-copy-into-test-" (number->string (getpid)))
         ) ;path-join
       ) ;base
       (src (path-join base "path-copy-into-src.txt"))
       (dir (path-join base "copy-into-dir"))
      ) ;
  (path-mkdir base)
  (path-write-text src "hello path-copy-into")
  (path-mkdir dir)
  (check-true (path-copy-into src dir))
  (check (path-read-text (path-join dir (path "path-copy-into-src.txt")))
    =>
    "hello path-copy-into"
  ) ;check
  (path-unlink src)
  (path-unlink (path-join dir (path "path-copy-into-src.txt")))
  (path-rmdir dir)
  (path-rmdir base)
) ;let*

(check-report)
