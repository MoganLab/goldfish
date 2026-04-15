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

;; gf doc 模糊匹配建议应输出完整命令
;; 当全局或库内查询没有精确命中但有建议时，输出不应只包含函数名，
;; 而应包含可直接执行的完整命令。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\"")
  ) ;os-call
) ;define

(define (cleanup-suggestion-command-fixture base-root
        ) ;cleanup-suggestion-command-fixture
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
    (path-unlink (path-join tests-root
                   "liii"
                   "demo"
                   "demo-prefix-string-test.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join tests-root
                   "liii"
                   "demo"
                   "demo-prefix-stringify-test.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "liii" "demo.scm")
      #t
    ) ;path-unlink
    (if (path-dir? (path-join tests-root "liii" "demo")
        ) ;path-dir?
      (path-rmdir (path-join tests-root "liii" "demo")
      ) ;path-rmdir
      #f
    ) ;if
    (if (path-dir? (path-join tests-root "liii")
        ) ;path-dir?
      (path-rmdir (path-join tests-root "liii")
      ) ;path-rmdir
      #f
    ) ;if
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
                      (string-append "golddoc-suggestion-command-"
                        (number->string (getpid))
                      ) ;string-append
                    ) ;path-join
         ) ;base-root
         (load-root (path-join base-root "goldfish")
         ) ;load-root
         (liii-root (path-join load-root "liii"))
         (tests-root (path-join base-root "tests")
         ) ;tests-root
         (group-root (path-join tests-root "liii")
         ) ;group-root
         (library-root (path-join group-root "demo")
         ) ;library-root
         (index-path (path-join tests-root
                       "function-library-index.json"
                     ) ;path-join
         ) ;index-path
         (global-output-path (path-join base-root "global.log")
         ) ;global-output-path
         (library-output-path (path-join base-root "library.log")
         ) ;library-output-path
         (command-name (path-name (executable)))
        ) ;
    (cleanup-suggestion-command-fixture base-root
    ) ;cleanup-suggestion-command-fixture
    (mkdir (path->string base-root))
    (mkdir (path->string load-root))
    (mkdir (path->string liii-root))
    (mkdir (path->string tests-root))
    (mkdir (path->string group-root))
    (mkdir (path->string library-root))
    (path-write-text (path-join liii-root "demo.scm")
      "(define-library (liii demo) (export) (import (scheme base)) (begin))"
    ) ;path-write-text
    (path-write-text (path-join library-root
                       "demo-prefix-string-test.scm"
                     ) ;path-join
      ";; demo-prefix-string\n(check-report)\n"
    ) ;path-write-text
    (path-write-text (path-join library-root
                       "demo-prefix-stringify-test.scm"
                     ) ;path-join
      ";; demo-prefix-stringify\n(check-report)\n"
    ) ;path-write-text
    (path-write-text index-path
      "{\"demo-prefix-string\":[\"(liii demo)\"],\"demo-prefix-stringify\":[\"(liii demo)\"]}"
    ) ;path-write-text
    (dynamic-wind (lambda ()
                    (path-unlink global-output-path #t)
                    (path-unlink library-output-path #t)
                  ) ;lambda
      (lambda ()
        (run-shell-command (string-append (executable)
                             " -I "
                             (path->string load-root)
                             " doc demo-prefix-str > "
                             (path->string global-output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (run-shell-command (string-append (executable)
                             " -I "
                             (path->string load-root)
                             " doc liii/demo demo-prefix-str > "
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
                        "Try one of these commands:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " doc \"demo-prefix-string\""
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? global-output
                        (string-append command-name
                          " doc \"demo-prefix-stringify\""
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        "Try one of these commands:"
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        (string-append command-name
                          " doc liii/demo \"demo-prefix-string\""
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
          (check-true (string-contains? library-output
                        (string-append command-name
                          " doc liii/demo \"demo-prefix-stringify\""
                        ) ;string-append
                      ) ;string-contains?
          ) ;check-true
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink global-output-path #t)
        (path-unlink library-output-path #t)
        (cleanup-suggestion-command-fixture base-root
        ) ;cleanup-suggestion-command-fixture
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
