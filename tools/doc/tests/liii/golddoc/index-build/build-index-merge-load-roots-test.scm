;; build-function-indexes!
;; 多个加载根目录指向同一个 tests 根目录时，索引必须合并后只写一次。
;;
;; 语法
;; ----
;; (build-function-indexes!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list?
;; 返回实际生成的 `function-library-index.json` 路径列表。
;;
;; 描述
;; ----
;; 库从 goldfish/ 迁移到 library/ 之后，两个加载根目录可能回退到同一个
;; tests 根目录（例如 library/tests 尚未建立时，library/ 与 goldfish/
;; 都回退到仓库级 tests/）。此时索引必须合并两个加载根目录的导出记录，
;; 否则后写入的一方会覆盖另一方的记录，导致部分函数无法检索。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc) (liii os) (liii path))

(check-set-mode! 'report-failed)

(define (cleanup-merge-fixture base-root)
  (let* ((load-root-a (path-join base-root "libroot-a"))
         (load-root-b (path-join base-root "libroot-b"))
         (tests-root (path-join base-root "tests"))
        ) ;
    (path-unlink (path-join tests-root "function-library-index.json") #t)
    (path-unlink (path-join load-root-a "liii" "alpha.scm") #t)
    (path-unlink (path-join load-root-b "liii" "beta.scm") #t)
    (if (path-dir? (path-join load-root-a "liii")) (path-rmdir (path-join load-root-a "liii")) #f)
    (if (path-dir? (path-join load-root-b "liii")) (path-rmdir (path-join load-root-b "liii")) #f)
    (if (path-dir? load-root-a) (path-rmdir load-root-a) #f)
    (if (path-dir? load-root-b) (path-rmdir load-root-b) #f)
    (if (path-dir? tests-root) (path-rmdir tests-root) #f)
    (if (path-dir? base-root) (path-rmdir base-root) #f)
  ) ;let
) ;define

(let* ((base-root
         (path-join (path-temp-dir)
           (string-append "golddoc-merge-load-roots-" (number->string (getpid)))
         ) ;path-join
       ) ;base-root
       (load-root-a (path-join base-root "libroot-a"))
       (load-root-b (path-join base-root "libroot-b"))
       (tests-root (path-join base-root "tests"))
       (old-load-path *load-path*)
      ) ;
  (cleanup-merge-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root-a))
  (mkdir (path->string (path-join load-root-a "liii")))
  (mkdir (path->string load-root-b))
  (mkdir (path->string (path-join load-root-b "liii")))
  (mkdir (path->string tests-root))
  (path-write-text (path-join load-root-a "liii" "alpha.scm")
    "(define-library (liii alpha)\n  (export alpha-run)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join load-root-b "liii" "beta.scm")
    "(define-library (liii beta)\n  (export beta-run)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (dynamic-wind
    (lambda ()
      (set! *load-path* (list (path->string load-root-a) (path->string load-root-b)))
    ) ;lambda
    (lambda ()
      (let ((built-paths (build-function-indexes!))
            (index-path (path->string (path-join tests-root "function-library-index.json")))
           ) ;
        (check built-paths => (list index-path))
        (check-true (path-file? index-path))
        (check (visible-libraries-for-function "alpha-run") => '("liii/alpha"))
        (check (visible-libraries-for-function "beta-run") => '("liii/beta"))
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-merge-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
