(import (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii sys)
) ;import
(check-set-mode! 'report-failed)
(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\""))
) ;define
(when (not (os-windows?))
  (let* ((bad-file (path-join (path-temp-dir)
                     (string-append "gf-fix-hint-"
                       (number->string (getpid))
                       ".scm"
                     ) ;string-append
                   ) ;path-join
         ) ;bad-file
         (output-path (path-join (path-temp-dir)
                        (string-append "gf-fix-hint-"
                          (number->string (getpid))
                          ".log"
                        ) ;string-append
                      ) ;path-join
         ) ;output-path
        ) ;
    (path-unlink bad-file #t)
    (path-unlink output-path #t)
    (dynamic-wind (lambda () (path-write-text bad-file "(define x 1))\n"))
      (lambda ()
        (check (not (= (run-shell-command (string-append (executable)
                                            " "
                                            (path->string bad-file)
                                            " > "
                                            (path->string output-path)
                                            " 2>&1"
                                          ) ;string-append
                       ) ;run-shell-command
                      0
                    ) ;=
               ) ;not
          =>
          #t
        ) ;check
        (let ((output (path-read-text output-path))
              (fix-hint (string-append "Hint: try `gf fix "
                          (path->string bad-file)
                          "` to repair common parenthesis issues."
                        ) ;string-append
              ) ;fix-hint
             ) ;
          (check-true (string-contains? output "unexpected close paren")
          ) ;check-true
          (check-true (string-contains? output fix-hint))
        ) ;let
      ) ;lambda
      (lambda ()
        (path-unlink bad-file #t)
        (path-unlink output-path #t)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when
(check-report)