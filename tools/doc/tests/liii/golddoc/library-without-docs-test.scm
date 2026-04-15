;; 添加 tools/golddoc 到 load path，以便导入 (liii golddoc)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/golddoc" *load-path*)
) ;set!

(import (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; gf doc 在库存在、但没有对应库级文档与测试用例时，
;; 应提示库文档不存在，并给出 gf source 命令。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\"")
  ) ;os-call
) ;define

(define (cleanup-library-without-docs-fixture base-root
        ) ;cleanup-library-without-docs-fixture
  (let ((load-root (path-join base-root "goldfish")
        ) ;load-root
        (tests-root (path-join base-root "tests")
        ) ;tests-root
       ) ;
    (path-unlink (path-join load-root "liii" "demo.scm")
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
                      (string-append "golddoc-library-without-docs-"
                        (number->string (getpid))
                      ) ;string-append
                    ) ;path-join
         ) ;base-root
         (load-root (path-join base-root "goldfish")
         ) ;load-root
         (liii-root (path-join load-root "liii"))
         (output-path (path-join base-root "library.log")
         ) ;output-path
         (command-name (path-name (executable)))
        ) ;
    (cleanup-library-without-docs-fixture base-root
    ) ;cleanup-library-without-docs-fixture
    (mkdir (path->string base-root))
    (mkdir (path->string load-root))
    (mkdir (path->string liii-root))
    (path-write-text (path-join liii-root "demo.scm")
      "(define-library (liii demo)\n  (export demo-value)\n  (import (scheme base))\n  (begin))\n"
    ) ;path-write-text
    (dynamic-wind (lambda () (path-unlink output-path #t))
      (lambda ()
        (run-shell-command (string-append (executable)
                             " -I "
                             (path->string load-root)
                             " doc liii/demo > "
                             (path->string output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (let ((output (path-read-text output-path)))
          (check-true (string-contains? output
                        "Library (liii demo) exists."
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? output
                        "No documentation and test cases available."
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? output
                        "Try one of these commands:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? output
                        (string-append command-name
                          " source liii/demo"
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
        (cleanup-library-without-docs-fixture base-root
        ) ;cleanup-library-without-docs-fixture
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
