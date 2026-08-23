(import (liii check)
        (liii project)
        (liii path)
        (liii string)
        (goldfish))

(check (let ((r (project-root))) (or (not r) (string? r))) => #t)

(check (let* ((tmp "/tmp/gf-project-test")
              (sub (string-append tmp "/a/b"))
              (gf (string-append tmp "/gfproject.scm"))
             ) ;
         (g_mkdir tmp)
         (g_mkdir (string-append tmp "/a"))
         (g_mkdir sub)
         (call-with-output-file gf (lambda (p) (display "(gfproject (tools))" p)))
         (let ((old (g_getcwd)))
           (g_chdir sub)
           (let ((r (project-root)))
             (g_chdir old)
             (g_remove-file gf)
             (equal? r tmp)
           ) ;let
         ) ;let
       ) => #t)

(check (let ((r (gfproject-tool-imports "test")))
         (and (list? r) (every string? r))) => #t)

(check (let ((r (gfproject-tool-imports "no-such-tool-xyz"))) (null? r)) => #t)

(check (let ((libs (function-libraries "string-append"))) (and (list? libs) (> (length libs) 0))) => #t)

(check (let ((libs (function-libraries "no-such-function-xyz"))) (list? libs)) => #t)

(check-report)
