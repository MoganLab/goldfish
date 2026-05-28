(import (liii check) (liii os) (liii subprocess))

(when (os-linux?)
  ;; Basic pipe
  (check (run-pipe "echo hello world" '("grep" "hello")) => "hello world\n")

  ;; Multiple stages
  (check (run-pipe "echo a\nb\nc" '("grep" "a") '("wc" "-l")) => "1\n")

  ;; Single command returns stdout
  (check (run-pipe "echo hello") => "hello\n")
) ;when

(check-report)
