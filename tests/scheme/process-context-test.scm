(import (srfi srfi-78)
  (scheme process-context)
  (srfi srfi-13)
  (liii os)
) ;import
(check-set-mode! 'report-failed)
(when (os-linux?)
  (check (string-prefix? "/"
           (get-environment-variable "HOME")
         ) ;string-prefix?
    =>
    #t
  ) ;check
) ;when
(when (os-linux?)
  (let ((envs (get-environment-variables)))
    (check (list? envs) => #t)
    (let ((home-env (assoc "HOME" envs)))
      (check (pair? home-env) => #t)
      (check (string-prefix? "/" (cdr home-env))
        =>
        #t
      ) ;check
    ) ;let
    (check (pair? (assoc "PATH" envs))
      =>
      #t
    ) ;check
  ) ;let
) ;when
(check-report)