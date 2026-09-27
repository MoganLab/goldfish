(import (liii check)
        (scheme base)
        (scheme file)
        (only (srfi srfi-13) string-prefix?)
        (only (liii string) string-join)
        (only (liii path) path path-join path->string)
        (only (liii os) os-sep))

(check-set-mode! 'report-failed)

;; A small cross-library program should preserve ordinary Scheme state and
;; control flow while using the selected SRFI and liii libraries together.
(define workflow-state 0)
(check (begin
         (set! workflow-state (+ workflow-state 1))
         workflow-state)
  => 1)

(check (guard (condition (else 'caught))
         (raise 'workflow-error))
  => 'caught)

(check (call-with-values
         (lambda () (values workflow-state "ready"))
         list)
  => '(1 "ready"))

(check (string-prefix? "native" (string-join '("native" " workflow") ""))
  => #t)

(check (path->string (path-join (path "tmp") "workflow.txt"))
  => (string-append "tmp" (string (os-sep)) "workflow.txt"))

(check (call-with-port (open-input-string "native workflow")
         (lambda (port) (read-string 6 port)))
  => "native")

(let ((workflow-file "tests/c3/native-workflow-tmp.txt"))
  (dynamic-wind
    (lambda ()
      (when (file-exists? workflow-file)
        (delete-file workflow-file)))
    (lambda ()
      (call-with-output-file workflow-file
        (lambda (port) (display "native workflow" port)))
      (check (call-with-input-file workflow-file
               (lambda (port) (read-string 6 port)))
        => "native"))
    (lambda ()
      (when (file-exists? workflow-file)
        (delete-file workflow-file)))))

(check-report)
