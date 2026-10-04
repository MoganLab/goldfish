(define-library (r7rs-audit include-case)
  (import (scheme base))
  (export included-value)
  (include-ci "tests/r7rs/fixtures/upper-body.scm"))
