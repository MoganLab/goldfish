;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/golddoc" *load-path*))

(import (liii check)
        (liii golddoc)
        (liii os)
        (liii path)
) ;import

(check-set-mode! 'report-failed)

;; build-function-indexes!
;; 根据当前 *load-path* 自动扫描测试文档并生成函数索引 JSON。
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
;; 该函数会沿着当前 *load-path* 推导关联的 `tests` 根目录，
;; 自动纳入除 `srfi` / `goldfish` 外的规范化测试目录，并写出索引文件。

(define (cleanup-build-index-fixture base-root)
  (let ((load-root (path-join base-root "goldfish"))
        (tests-root (path-join base-root "tests")))
    (path-unlink (path-join tests-root "function-library-index.json") #t)
    (path-unlink (path-join load-root "liii" "alpha.scm") #t)
    (path-unlink (path-join load-root "custom" "beta.scm") #t)
    (path-unlink (path-join load-root "custom" "gamma.scm") #t)
    (path-unlink (path-join load-root "srfi" "1.scm") #t)
    (path-unlink (path-join tests-root "liii" "alpha" "alpha-equal-p-test.scm") #t)
    (path-unlink (path-join tests-root "liii" "alpha" "shared-value-test.scm") #t)
    (path-unlink (path-join tests-root "custom" "beta" "beta-search-test.scm") #t)
    (path-unlink (path-join tests-root "custom" "gamma" "shared-value-test.scm") #t)
    (path-unlink (path-join tests-root "srfi" "1" "skip-me-test.scm") #t)
    (if (path-dir? (path-join tests-root "liii" "alpha"))
        (path-rmdir (path-join tests-root "liii" "alpha"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "liii"))
        (path-rmdir (path-join tests-root "liii"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "custom" "beta"))
        (path-rmdir (path-join tests-root "custom" "beta"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "custom" "gamma"))
        (path-rmdir (path-join tests-root "custom" "gamma"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "custom"))
        (path-rmdir (path-join tests-root "custom"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "srfi" "1"))
        (path-rmdir (path-join tests-root "srfi" "1"))
        #f
    ) ;if
    (if (path-dir? (path-join tests-root "srfi"))
        (path-rmdir (path-join tests-root "srfi"))
        #f
    ) ;if
    (if (path-dir? tests-root)
        (path-rmdir tests-root)
        #f
    ) ;if
    (if (path-dir? (path-join load-root "liii"))
        (path-rmdir (path-join load-root "liii"))
        #f
    ) ;if
    (if (path-dir? (path-join load-root "custom"))
        (path-rmdir (path-join load-root "custom"))
        #f
    ) ;if
    (if (path-dir? (path-join load-root "srfi"))
        (path-rmdir (path-join load-root "srfi"))
        #f
    ) ;if
    (if (path-dir? load-root)
        (path-rmdir load-root)
        #f
    ) ;if
    (if (path-dir? base-root)
        (path-rmdir base-root)
        #f
    ) ;if
  ) ;let
) ;define

(let* ((base-root (path-join (path-temp-dir)
                             (string-append "golddoc-build-index-"
                                            (number->string (getpid)))))
       (load-root (path-join base-root "goldfish"))
       (tests-root (path-join base-root "tests"))
       (old-load-path *load-path*))
  (cleanup-build-index-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string (path-join load-root "liii")))
  (mkdir (path->string (path-join load-root "custom")))
  (mkdir (path->string (path-join load-root "srfi")))
  (mkdir (path->string tests-root))
  (mkdir (path->string (path-join tests-root "liii")))
  (mkdir (path->string (path-join tests-root "liii" "alpha")))
  (mkdir (path->string (path-join tests-root "custom")))
  (mkdir (path->string (path-join tests-root "custom" "beta")))
  (mkdir (path->string (path-join tests-root "custom" "gamma")))
  (mkdir (path->string (path-join tests-root "srfi")))
  (mkdir (path->string (path-join tests-root "srfi" "1")))
  (path-write-text (path-join load-root "liii" "alpha.scm")
                   "(define-library (liii alpha) (export) (import (scheme base)) (begin))")
  (path-write-text (path-join load-root "custom" "beta.scm")
                   "(define-library (custom beta) (export) (import (scheme base)) (begin))")
  (path-write-text (path-join load-root "custom" "gamma.scm")
                   "(define-library (custom gamma) (export) (import (scheme base)) (begin))")
  (path-write-text (path-join load-root "srfi" "1.scm")
                   "(define-library (srfi 1) (export) (import (scheme base)) (begin))")
  (path-write-text (path-join tests-root "liii" "alpha" "alpha-equal-p-test.scm")
                   ";; alpha=?\n;; (alpha=? a b)\n")
  (path-write-text (path-join tests-root "liii" "alpha" "shared-value-test.scm")
                   ";; shared-value\n;; (shared-value alpha)\n")
  (path-write-text (path-join tests-root "custom" "beta" "beta-search-test.scm")
                   ";; beta-search! 函数测试\n;; (beta-search! beta element)\n")
  (path-write-text (path-join tests-root "custom" "gamma" "shared-value-test.scm")
                   ";; shared-value\n;; (shared-value gamma)\n")
  (path-write-text (path-join tests-root "srfi" "1" "skip-me-test.scm")
                   ";; skip-me\n;; (skip-me x)\n")
  (dynamic-wind
    (lambda ()
      (set! *load-path* (list (path->string load-root)))
    ) ;lambda
    (lambda ()
      (let ((built-paths (build-function-indexes!))
            (index-path (path->string (path-join tests-root "function-library-index.json"))))
        (check built-paths => (list index-path))
        (check-true (path-file? index-path))
        (check (visible-libraries-for-function "alpha=?") => '("liii/alpha"))
        (check (visible-libraries-for-function "beta-search!") => '("custom/beta"))
        (check (visible-libraries-for-function "shared-value") => '("custom/gamma" "liii/alpha"))
        (check (visible-libraries-for-function "skip-me") => '())
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-build-index-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
