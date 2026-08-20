(import (liii check) (liii string) (goldfish))

;; Host ABI regression: the R7RS value surface (host-abi.scm) is loaded by the
;; seed into the rootlet; these names must resolve without any library import
;; beyond (liii check).  The explicit load-source-file is idempotent (the seed
;; already loaded the file) and documents the single source.

(check (min 3 1 2) => 1)
(check (max 3 1 2) => 3)
(check (exact-integer? 5) => #t)
(check (finite? 1.5) => #t)
(check (finite? 1e400) => #f)
(check (string-upcase "abc") => "ABC")
(check (string-downcase "AbC") => "abc")
(check (char-upcase #\a) => #\A)
(check (char-ci=? #\a #\A) => #t)
(check (string-ci=? "abc" "ABC") => #t)
(check (bytevector-copy! (make-bytevector 3) 0 (bytevector 1 2 3)) => (bytevector 1 2 3))
(check (promise? (make-promise 5)) => #t)
(check (force (make-promise 5)) => 5)
(check (truncate-quotient 7 2) => 3)
(check (truncate-remainder 7 2) => 1)
(check (eof-object? (read-u8 (open-input-bytevector (bytevector 1 2)))) => #f)
(check (get-output-bytevector (begin (write-u8 65 (open-output-bytevector)) (open-output-bytevector))) => (bytevector))
(check-report)
