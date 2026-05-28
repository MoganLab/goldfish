(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  (let-values (((out err code) (run-values "echo hello")))
    (check out => "hello\n")
    (check err => "")
    (check (zero? code) => #t)
  )

  (let-values (((out err code) (run-values "false")))
    (check out => "")
    (check (zero? code) => #f)
  )
)

(check-report)
