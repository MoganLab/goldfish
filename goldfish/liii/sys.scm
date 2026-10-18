(define-library (liii sys)
  (export argv executable which stdin-tty? stdout-tty? stderr-tty? sleep)
  (import (scheme process-context) (liii os) (only (liii time) sleep))
  (begin
    (define (argv)
      (command-line)
    ) ;define

    (define (executable)
      (g_executable)
    ) ;define

    (define* (which cmd (path #f)) (if path (g_which cmd path) (g_which cmd)))

    (define (stdin-tty?)
      (os-tty? 0)
    ) ;define

    (define (stdout-tty?)
      (os-tty? 1)
    ) ;define

    (define (stderr-tty?)
      (os-tty? 2)
    ) ;define

  ) ;begin
) ;define-library
