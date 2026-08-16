(import (rename (extensible-match-test)
                (run-tests run-match-tests))
        (rename (extensible-match internal-tests)
                (run-tests run-internal-tests))
        (chibi test))
(test-group "all tests"
  (test-group "API tests"
    (run-match-tests))
  (test-group "internal tests"
    (run-internal-tests)))
(test-exit)
