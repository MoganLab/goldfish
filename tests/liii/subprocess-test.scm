(import (liii check)
  (liii os)
  (liii subprocess)
  (liii string))

(check-set-mode! 'report-failed)

;; Test run returns exit code
(check (run "python3" "-c" "import sys; sys.exit(0)") => 0)
(check (> (run "python3" "-c" "import sys; sys.exit(42)") 0) => #t)

;; Test run-string captures stdout
(check (run-string "python3" "-c" "print('hello')")
  => (if (os-windows?) "hello\r\n" "hello\n"))

;; Test run-strings splits lines
(let-values (((out err code) (run-strings "python3" "-c" "print('line1'); print('line2')")))
  (check out => (if (os-windows?) '("line1" "line2" "") '("line1" "line2" "")))
  (check err => '())
  (check code => 0))

;; Test :input
(check (run-string "python3" "-c" "import sys; sys.stdout.write(sys.stdin.read())"
         :input "hello")
  => "hello")

;; Test :cwd
(check (run-string "python3" "-c" "import os; print(os.getcwd())"
         :cwd "/tmp")
  => (if (os-windows?) "C:\\tmp\r\n" "/tmp\n"))

;; Test :env
(check (run-string "python3" "-c" "import os; print(os.environ.get('GF_TEST_VAR', ''))"
         :env '(("GF_TEST_VAR" . "goldfish")))
  => (if (os-windows?) "goldfish\r\n" "goldfish\n"))

;; Test :stderr 'stdout merges stderr into stdout
;; Note: on Windows the merged order may be stdout-before-stderr due to
;; separate temp files; we only check that both contents are present.
(let ((merged (run-string "python3" "-u" "-c" "import sys; sys.stdout.write('out'); sys.stderr.write('err')"
              :stderr 'stdout)))
  (check (string-contains? merged "out") => #t)
  (check (string-contains? merged "err") => #t))

;; Test :stderr 'string captures stderr separately in run-strings
(let-values (((out err code) (run-strings "python3" "-c" "import sys; sys.stdout.write('ok'); sys.stderr.write('err')"
               :stderr 'string)))
  (check out => '("ok"))
  (check err => '("err"))
  (check code => 0))

;; Test :search #f requires absolute path
(check-catch 'value-error (run-string "python3" "-c" "print('hello')" :search #f))

;; Test command not found throws exception
(check-catch 'os-error (run-string "a_command_that_does_not_exist_12345"))

(check-report)
