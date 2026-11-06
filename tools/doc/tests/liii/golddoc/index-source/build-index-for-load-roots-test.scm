;; build-index-for-load-roots
;; 扫描若干个库加载根目录，合并各根目录下库源码导出的函数索引。
;;
;; 语法
;; ----
;; (build-index-for-load-roots load-roots)
;;
;; 参数
;; ----
;; load-roots : list of string
;;   库源码加载根目录列表。
;;
;; 返回值
;; ----
;; list?
;; 形如 `((函数名 . ("(库名)" ...)) ...)` 的索引，按函数名与库名排序。
;;
;; 描述
;; ----
;; 每个加载根目录按分组目录扫描 `(define-library ... (export ...))`，
;; 只收录库名首段与分组目录同名的库；`tests` 目录与 `*-test.scm` 会被跳过。
;; 多个根目录的记录合并去重后返回。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc index-source) (liii os) (liii path))

(check-set-mode! 'report-failed)

(define (cleanup-load-roots-fixture base-root)
  (let* ((load-root (path-join base-root "libroot"))
         (lib-dir (path-join load-root "liii"))
        ) ;
    (path-unlink (path-join lib-dir "alpha.scm") #t)
    (path-unlink (path-join lib-dir "beta.scm") #t)
    (if (path-dir? lib-dir) (path-rmdir lib-dir) #f)
    (if (path-dir? load-root) (path-rmdir load-root) #f)
    (if (path-dir? base-root) (path-rmdir base-root) #f)
  ) ;let
) ;define

(let* ((base-root
         (path-join (path-temp-dir)
           (string-append "golddoc-index-source-" (number->string (getpid)))
         ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "libroot"))
       (lib-dir (path-join load-root "liii"))
      ) ;
  (cleanup-load-roots-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string lib-dir))
  (path-write-text (path-join lib-dir "alpha.scm")
    "(define-library (liii alpha)\n  (export alpha-run shared-value)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join lib-dir "beta.scm")
    "(define-library (liii beta)\n  (export beta-run shared-value)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (let ((index (build-index-for-load-roots (list (path->string load-root)))))
    (check (assoc "alpha-run" index) => (cons "alpha-run" (list "(liii alpha)")))
    (check (assoc "beta-run" index) => (cons "beta-run" (list "(liii beta)")))
    (check (assoc "shared-value" index)
      =>
      (cons "shared-value" (list "(liii alpha)" "(liii beta)"))
    ) ;check
  ) ;let
  (cleanup-load-roots-fixture base-root)
) ;let*

(check-report)
