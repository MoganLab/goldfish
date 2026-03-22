(import (liii check)
        (liii os)
) ;import

;; Only run this test when GOLDFISH_TEST_STACKTRACE is set
(when (let ((env (getenv "GOLDFISH_TEST_STACKTRACE")))
        (and env (not (equal? env "0"))))
  ;; Simple test for stacktrace display on failure
  (check-set-mode! 'report-failed)

  ;; Test basic failure
  (check (+ 1 1) => 3)  ; Should show stacktrace

  (check-report)
) ;when