;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check)
  (liii golddoc)
  (liii os)
  (liii path)
  (liii string)
  (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; gf doc 在函数已被 export、但没有对应函数级文档与测试用例时，
;; 应输出专门提示，并给出可继续尝试的库级命令。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\"")
  ) ;os-call
) ;define

(define (cleanup-exported-without-docs-fixture base-root
        ) ;cleanup-exported-without-docs-fixture
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
    (path-unlink (path-join load-root "liii" "demo.scm")
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root
                   "custom"
                   "other.scm"
                 ) ;path-join
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

(when (not (os-windows?))
  (let* ((base-root (path-join (path-temp-dir)
                      (string-append "golddoc-exported-without-docs-"
                        (number->string (getpid))
                      ) ;string-append
                    ) ;path-join
         ) ;base-root
         (load-root (path-join base-root "goldfish")
         ) ;load-root
         (liii-root (path-join load-root "liii"))
         (custom-root (path-join load-root "custom")
         ) ;custom-root
         (tests-root (path-join base-root "tests")
         ) ;tests-root
         (global-output-path (path-join base-root "global.log")
         ) ;global-output-path
         (library-output-path (path-join base-root "library.log")
         ) ;library-output-path
         (command-name (path-name (executable)))
         (old-load-path *load-path*)
        ) ;
    (cleanup-exported-without-docs-fixture base-root
    ) ;cleanup-exported-without-docs-fixture
    (mkdir (path->string base-root))
    (mkdir (path->string load-root))
    (mkdir (path->string liii-root))
    (mkdir (path->string custom-root))
    (mkdir (path->string tests-root))
    (path-write-text (path-join liii-root "demo.scm")
      "(define-library (liii demo)\n  (export exported-missing)\n  (import (scheme base))\n  (begin))\n"
    ) ;path-write-text
    (path-write-text (path-join custom-root "other.scm")
      "(define-library (custom other)\n  (export exported-missing)\n  (import (scheme base))\n  (begin))\n"
    ) ;path-write-text
    (dynamic-wind (lambda ()
                    (path-unlink global-output-path #t)
                    (path-unlink library-output-path #t)
                    (set! *load-path*
                      (list (path->string load-root))
                    ) ;set!
                  ) ;lambda
      (lambda ()
        (build-function-indexes!)
        (run-shell-command (string-append (executable)
                             " -I "
                             (path->string load-root)
                             " doc exported-missing > "
                             (path->string global-output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (run-shell-command (string-append (executable)
                             " -I "
                             (path->string load-root)
                             " doc liii/demo exported-missing > "
                             (path->string library-output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (let ((global-output (path-read-text global-output-path)
              ) ;global-output
              (library-output (path-read-text library-output-path)
              ) ;library-output
             ) ;
          (check-true (string-contains? global-output
                        "Function exported-missing is exported in:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        "  (liii demo)"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        "  (custom other)"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        "No documentation and test cases available."
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        "Try one of these commands:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " doc liii/demo"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " source liii/demo"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " doc custom/other"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " source custom/other"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        "Function exported-missing is exported in:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        "  (liii demo)"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        "No documentation and test cases available."
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        (string-append command-name
                          " doc liii/demo"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        (string-append command-name
                          " source liii/demo"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (set! *load-path* old-load-path)
        (path-unlink global-output-path #t)
        (path-unlink library-output-path #t)
        (cleanup-exported-without-docs-fixture base-root
        ) ;cleanup-exported-without-docs-fixture
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
