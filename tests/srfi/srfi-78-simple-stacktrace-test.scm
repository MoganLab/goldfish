(import (liii check) (liii os))

;; Only run this test when GOLDFISH_TEST_STACKTRACE is set
(when (let ((env (getenv "GOLDFISH_TEST_STACKTRACE")
            ) ;env
           ) ;
        (and env (not (equal? env "0")))
      ) ;let
  ;; Simple test for stacktrace display on failure
  (check-set-mode! 'report-failed)

  ;; Test basic failure
  (check (+ 1 1) => 3)

  (check-report)
) ;when
