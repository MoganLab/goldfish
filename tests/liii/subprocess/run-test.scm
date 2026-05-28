(import (liii check) (liii os) (liii subprocess))

(check (zero? (run "true")) => #t)
(check (zero? (run "false")) => #f)

(check (run "echo hello") => 0)

(check (zero? (run '("true"))) => #t)
(check (zero? (run '("false"))) => #f)

(let ((orig-dir (getcwd)))
  (run "pwd" :cwd "/tmp")
  (check (getcwd) => orig-dir)
) ;let

(let ((orig-dir (getcwd)))
  (run '("pwd") :cwd "/tmp")
  (check (getcwd) => orig-dir)
) ;let

(check-report)
