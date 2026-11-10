;; path-copy
;; 将文件复制到目标路径。
;;
;; 语法
;; ----
;; (path-copy source target)
;;
;; 参数
;; ----
;; source : path | string
;; 源文件路径。
;; target : path | string
;; 目标文件路径。
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
           (string-append "gf-path-copy-test-" (number->string (getpid)))
         ) ;path-join
       ) ;base
       (src (path-join base "path-copy-src.txt"))
       (dst (path-join base "path-copy-dst.txt"))
      ) ;
  (path-mkdir base)
  (path-write-text src "hello path-copy")
  (check-true (path-copy src dst))
  (check (path-read-text dst) => "hello path-copy")
  (path-unlink src)
  (path-unlink dst)
  (path-rmdir base)
) ;let*

;; 错误测试
(check-catch 'type-error (path-copy #\a "x"))
(check-catch 'type-error (path-copy "x" #\a))
(check-catch 'type-error (path-copy 123 "x"))
(check-catch 'type-error (path-copy "x" 123))

(check-report)
