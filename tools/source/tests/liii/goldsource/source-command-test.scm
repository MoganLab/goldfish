;; 添加 tools/goldsource 到 load path，以便导入 (liii goldsource)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/goldsource" *load-path*))

(import (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; gf source 应直接输出库源代码，并在库不存在时报错。

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(define (cleanup-source-command-fixture base-root)
  (let ((load-root (path-join base-root "goldfish")))
    (path-unlink (path-join load-root "liii" "demo.scm") #t)
    (if (path-dir? (path-join load-root "liii"))
        (path-rmdir (path-join load-root "liii"))
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
  (let*
    ((base-root
       (path-join (path-temp-dir)
                  (string-append "goldsource-command-"
                                 (number->string (getpid))
                  ) ;string-append
       ) ;path-join
     ) ;base-root
     (load-root (path-join base-root "goldfish"))
     (liii-root (path-join load-root "liii"))
     (source-path (path-join liii-root "demo.scm"))
     (output-path (path-join base-root "source.log"))
     (error-path (path-join base-root "missing.log"))
     (expected-source "(define-library (liii demo)\n  (export demo-value)\n  (import (scheme base))\n  (begin\n    (define demo-value 42)\n  ) ;begin\n) ;define-library\n")
    ) ;
    (cleanup-source-command-fixture base-root)
    (mkdir (path->string base-root))
    (mkdir (path->string load-root))
    (mkdir (path->string liii-root))
    (path-write-text source-path expected-source)
    (dynamic-wind
      (lambda ()
        (path-unlink output-path #t)
        (path-unlink error-path #t)
      ) ;lambda
      (lambda ()
        (run-shell-command (string-append (executable)
                                          " -I "
                                          (path->string load-root)
                                          " source liii/demo > "
                                          (path->string output-path)
                                          " 2>&1")
        ) ;run-shell-command
        (run-shell-command (string-append (executable)
                                          " -I "
                                          (path->string load-root)
                                          " source liii/missing > "
                                          (path->string error-path)
                                          " 2>&1")
        ) ;run-shell-command
        (check (path-read-text output-path) => expected-source)
        (check-true (string-contains? (path-read-text error-path)
                                      "Error: library not found in *load-path*: liii/missing")
        ) ;check-true
      ) ;lambda
      (lambda ()
        (path-unlink output-path #t)
        (path-unlink error-path #t)
        (cleanup-source-command-fixture base-root)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)
