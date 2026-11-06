;; build-function-indexes!
;; 索引构建必须跳过加载根目录下的 tests/ 目录。
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
;; 库迁移到 library/ 之后，tests/ 会与库源码同处一个加载根目录下。
;; tests/ 下的 *-test.scm 不是库定义，部分测试文件还使用了 #"" 原始字符串
;; 等读取扩展，被当作库源码解析时会让整个索引构建失败，
;; 因此扫描加载根目录时必须跳过 tests/ 目录。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc) (liii os) (liii path))

(check-set-mode! 'report-failed)

(define (cleanup-index-fixture base-root)
  (let* ((load-root (path-join base-root "libroot"))
         (tests-root (path-join load-root "tests"))
        ) ;
    (path-unlink (path-join tests-root "function-library-index.json") #t)
    (path-unlink (path-join tests-root "liii" "alpha-test.scm") #t)
    (if (path-dir? (path-join tests-root "liii"))
      (path-rmdir (path-join tests-root "liii"))
      #f
    ) ;if
    (if (path-dir? tests-root) (path-rmdir tests-root) #f)
    (path-unlink (path-join load-root "liii" "alpha.scm") #t)
    (if (path-dir? (path-join load-root "liii")) (path-rmdir (path-join load-root "liii")) #f)
    (if (path-dir? load-root) (path-rmdir load-root) #f)
    (if (path-dir? base-root) (path-rmdir base-root) #f)
  ) ;let
) ;define

(let* ((base-root
         (path-join (path-temp-dir)
           (string-append "golddoc-skip-tests-dir-" (number->string (getpid)))
         ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "libroot"))
       (tests-root (path-join load-root "tests"))
       (old-load-path *load-path*)
      ) ;
  (cleanup-index-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string (path-join load-root "liii")))
  (mkdir (path->string tests-root))
  (mkdir (path->string (path-join tests-root "liii")))
  (path-write-text (path-join load-root "liii" "alpha.scm")
    "(define-library (liii alpha)\n  (export alpha-run)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  ;; 该测试文件使用 #"" 原始字符串：普通 read 无法解析，
  ;; 若索引构建把 tests/ 当作库源码目录扫描，整个构建会失败。
  (path-write-text (path-join tests-root "liii" "alpha-test.scm")
    "(define raw #\"\"abc\"\")\n"
  ) ;path-write-text
  (dynamic-wind
    (lambda () (set! *load-path* (list (path->string load-root))))
    (lambda ()
      (let ((built-paths (build-function-indexes!))
            (index-path (path->string (path-join tests-root "function-library-index.json")))
           ) ;
        (check built-paths => (list index-path))
        (check-true (path-file? index-path))
        (check (visible-libraries-for-function "alpha-run") => '("liii/alpha"))
        (check (visible-libraries-for-function "raw") => '())
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-index-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
