(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  (check (run-string "echo hello") => "hello\n")
  (check (run-string '("echo" "hello")) => "hello\n")

  (check-catch 'value-error (run-string "false"))

  (check (run-string "pwd" :cwd "/tmp") => "/tmp\n")
) ;when

(check-report)
