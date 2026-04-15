(import (liii check)
  (liii os)
  (liii path)
  (liii string)
) ;import
(check-set-mode! 'report-failed)
(define (cleanup-function-libraries-fixture base-root
        ) ;cleanup-function-libraries-fixture
  (let ((load-root (path-join base-root "goldfish")
        ) ;load-root
        (hidden-root (path-join base-root "hidden-goldfish")
        ) ;hidden-root
       ) ;
    (path-unlink (path-join load-root "liii" "alpha.scm")
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root
                   "custom"
                   "beta.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
    (path-unlink (path-join load-root "srfi" "1.scm")
      #t
    ) ;path-unlink
    (path-unlink (path-join hidden-root
                   "liii"
                   "missing.scm"
                 ) ;path-join
      #t
    ) ;path-unlink
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
    (if (path-dir? (path-join hidden-root "liii")
        ) ;path-dir?
      (path-rmdir (path-join hidden-root "liii")
      ) ;path-rmdir
      #f
    ) ;if
    (if (path-dir? hidden-root)
      (path-rmdir hidden-root)
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
                    (string-append "goldfish-function-libraries-"
                      (number->string (getpid))
                    ) ;string-append
                  ) ;path-join
       ) ;base-root
       (load-root (path-join base-root "goldfish")
       ) ;load-root
       (hidden-root (path-join base-root "hidden-goldfish")
       ) ;hidden-root
       (liii-root (path-join load-root "liii"))
       (custom-root (path-join load-root "custom")
       ) ;custom-root
       (srfi-root (path-join load-root "srfi"))
       (hidden-liii-root (path-join hidden-root "liii")
       ) ;hidden-liii-root
       (old-load-path *load-path*)
      ) ;
  (cleanup-function-libraries-fixture base-root
  ) ;cleanup-function-libraries-fixture
  (mkdir (path->string base-root))
  (mkdir (path->string load-root))
  (mkdir (path->string hidden-root))
  (mkdir (path->string liii-root))
  (mkdir (path->string custom-root))
  (mkdir (path->string srfi-root))
  (mkdir (path->string hidden-liii-root))
  (path-write-text (path-join liii-root "alpha.scm")
    "(define-library (liii alpha)\n  (export unique-func shared-func duplicate-func)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join custom-root "beta.scm")
    "(define-library (custom beta)\n  (export shared-func (rename beta-hidden renamed-func))\n  (import (scheme base))\n  (begin (define beta-hidden 1))\n)\n"
  ) ;path-write-text
  (path-write-text (path-join srfi-root "1.scm")
    "(define-library (srfi 1)\n  (export fold)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (path-write-text (path-join hidden-liii-root
                     "missing.scm"
                   ) ;path-join
    "(define-library (liii missing)\n  (export invisible-func)\n  (import (scheme base))\n  (begin))\n"
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path*
                    (list (path->string load-root))
                  ) ;set!
                ) ;lambda
    (lambda ()
      (check (g_function-libraries "unique-func")
        =>
        '((liii alpha))
      ) ;check
      (check (g_function-libraries "shared-func")
        =>
        '((custom beta) (liii alpha))
      ) ;check
      (check (g_function-libraries "fold")
        =>
        '((srfi 1))
      ) ;check
      (check (g_function-libraries "renamed-func")
        =>
        '((custom beta))
      ) ;check
      (check (g_function-libraries "invisible-func")
        =>
        '()
      ) ;check
      (check (g_function-libraries "missing-func")
        =>
        '()
      ) ;check
      (check (g_function-libraries "duplicate-func")
        =>
        '((liii alpha))
      ) ;check
      (check-catch 'type-error
        (g_function-libraries 1)
      ) ;check-catch
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (cleanup-function-libraries-fixture base-root
      ) ;cleanup-function-libraries-fixture
    ) ;lambda
  ) ;dynamic-wind
) ;let*
(check-report)
