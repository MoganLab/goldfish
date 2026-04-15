;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check)
  (liii golddoc)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; build-function-indexes!
;; 根据当前 *load-path* 自动扫描源代码库的 export 声明并生成函数索引 JSON。
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
;; 从对应源代码目录的 `(define-library ... (export ...))` 中提取导出函数，
;; 并写出索引文件。

(define (cleanup-build-index-fixture base-root)
  (let ((load-root (path-join base-root "goldfish")
        ) ;load-root
        (tests-root (path-join base-root "tests")
        ) ;tests-root
       ) ;
    (path-unlink (path-join tests-root
                   "function-library-index.json"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "liii" "alpha.scm")
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root
                   "custom"
                   "beta.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root
                   "custom"
                   "gamma.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root
                   "custom"
                   "not-a-library.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "srfi" "1.scm")
      #t
    ) ;path-unlink
    (if (path-dir? tests-root)
      (path-rmdir tests-root)
      #f
    ) ;if
    (if (path-dir? (path-join load-root "liii"))
      (path-rmdir (path-join load-root "liii")
      ) ;path-rmdir
      #f
    ) ;if
    (if (path-dir? (path-join load-root "custom")
        ) ;path-dir?
      (path-rmdir (path-join load-root "custom")
      ) ;path-rmdir
      #f
    ) ;if
    (if (path-dir? (path-join load-root "srfi"))
      (path-rmdir (path-join load-root "srfi")
      ) ;path-rmdir
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
                      (number->string (getpid))
                    ) ;string-append
                  ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "goldfish")
       ) ;load-root
       (tests-root (path-join base-root "tests")
       ) ;tests-root
       (old-load-path *load-path*)
      ) ;
  (cleanup-build-index-fixture base-root)
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string (path-join load-root "liii")
         ) ;path->string
  ) ;mkdir
  (mkdir (path->string (path-join load-root "custom")
         ) ;path->string
  ) ;mkdir
  (mkdir (path->string (path-join load-root "srfi")
         ) ;path->string
  ) ;mkdir
  (mkdir (path->string tests-root))
  (path-write-text (path-join load-root "liii" "alpha.scm")
    "(define-library (liii alpha)\n  (export alpha=? shared-value)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join load-root
                     "custom"
                     "beta.scm"
                   ) ;path-join
    "(define-library (custom beta)\n  (export beta-search!)\n  (export beta-extra)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join load-root
                     "custom"
                     "gamma.scm"
                   ) ;path-join
    "(define-library (custom gamma)\n  (export shared-value)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join load-root
                     "custom"
                     "not-a-library.scm"
                   ) ;path-join
    "(define placeholder 1)\n"
  ) ;path-write-text
  (path-write-text (path-join load-root "srfi" "1.scm")
    "(define-library (srfi 1)\n  (export skip-me)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path*
                    (list (path->string load-root))
                  ) ;set!
                ) ;lambda
    (lambda ()
      (let ((built-paths (build-function-indexes!))
            (index-path (path->string (path-join tests-root
                                        "function-library-index.json"
                                      ) ;path-join
                        ) ;path->string
            ) ;index-path
           ) ;
        (check built-paths => (list index-path))
        (check-true (path-file? index-path))
        (check (visible-libraries-for-function "alpha=?"
               ) ;visible-libraries-for-function
          =>
          '("liii/alpha")
        ) ;check
        (check (visible-libraries-for-function "beta-search!"
               ) ;visible-libraries-for-function
          =>
          '("custom/beta")
        ) ;check
        (check (visible-libraries-for-function "beta-extra"
               ) ;visible-libraries-for-function
          =>
          '("custom/beta")
        ) ;check
        (check (visible-libraries-for-function "shared-value"
               ) ;visible-libraries-for-function
          =>
          '("custom/gamma" "liii/alpha")
        ) ;check
        (check (visible-libraries-for-function "skip-me"
               ) ;visible-libraries-for-function
          =>
          '("srfi/1")
        ) ;check
        (check (visible-libraries-for-function "placeholder"
               ) ;visible-libraries-for-function
          =>
          '()
        ) ;check
      ) ;let
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-build-index-fixture base-root)
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
