(import (liii list)
  (liii check)
  (liii os)
) ;import
(check-set-mode! 'report-failed)
(when (not (os-windows?))
  (check (file-exists? "/tmp") => #t)
  (check (file-exists? "/not_exists")
    =>
    #f
  ) ;check
) ;when
(when (and (os-linux?)
        (not (string=? "root" (getlogin)))
      ) ;and
  (check-catch 'permission-error
    (file-exists? "/root")
  ) ;check-catch
) ;when
(when (os-windows?)
  (check (file-exists? "C:") => #t)
) ;when
(when (and (os-linux?)
        (not (string=? "root" (getlogin)))
      ) ;and
  (check-catch 'permission-error
    (delete-file "/root")
  ) ;check-catch
) ;when
(when (not (os-windows?))
  (with-output-to-file "/tmp/test_delete_file"
    (lambda () (display "Hello, World!"))
  ) ;with-output-to-file
  (check (file-exists? "/tmp/test_delete_file")
    =>
    #t
  ) ;check
  (delete-file "/tmp/test_delete_file")
  (check (file-exists? "/tmp/test_delete_file")
    =>
    #f
  ) ;check
) ;when
(define (sum start end)
  (if (= start end)
    start
    (+ (sum start (- end 1)) end)
  ) ;if
) ;define
(check (sum 2 4) => 9)
(check-report "\n\nCheck report of boot-test.scm => "
) ;check-report