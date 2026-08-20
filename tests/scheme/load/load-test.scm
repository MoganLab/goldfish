(import (liii check) (liii os) (liii path) (scheme load))
(check-set-mode! 'report-failed)

;; load
;; 从文件中读取并求值 Scheme 表达式。
;;
;; 语法
;; ----
;; (load filename)
;;
;; 说明
;; ----
;; load 按顺序读取并求值文件中的全部顶层表达式，文件按 *load-path* 查找。

(check (procedure? load) => #t)

(let* ((base-root (path-join (path-temp-dir)
                    (string-append "goldfish-scheme-load-" (number->string (getpid)))
                  ) ;path-join
       ) ;base-root
       (file (path-join base-root "data.scm"))
      ) ;
  (path-write-text file "(display 1)\n")
  (check-catch #t (load file))
  (path-unlink file #t)
  (if (path-dir? base-root) (path-rmdir base-root) #f)
) ;let*

;; load 不存在的文件报错
(check-catch #t
  (load (path-join (path-temp-dir) "goldfish-no-such-file-xyz.scm"))
) ;check-catch

(check-report)
