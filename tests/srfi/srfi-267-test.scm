(import (liii check) (srfi srfi-267))

(check-set-mode! 'report-failed)

(define (capture-read-error thunk)
  (guard (err ((raw-string-read-error? err) err) (else 'unexpected-error))
    (thunk)
    'no-error
  ) ;guard
) ;define

(define (capture-write-error thunk)
  (guard (err ((raw-string-write-error? err) err) (else 'unexpected-error))
    (thunk)
    'no-error
  ) ;guard
) ;define

;; =======================================
;; Reader syntax
;; =======================================

(check #"""" => "")
(check #""a"" => "a")
(check #"-"""-" => "\"")
(check #"tag with space"hello"tag with space" => "hello")
(check #"(())"#""a"""(())" => "#\"\"a\"\"")

(let ((port (open-input-string "#\"\"\"\"\"")))
  (check (read-raw-string port) => "")
  (check (read-char port) => #\")
) ;let
(check-false (raw-string-read-error? 'plain-symbol))
(let ((err (capture-read-error (lambda () (read-raw-string (open-input-string "#x"))))
      ) ;err
     ) ;
  (check (raw-string-read-error? err) => #t)
) ;let
(check-false (raw-string-write-error? 'plain-symbol))
(let ((err (capture-write-error (lambda () (write-raw-string "\"" "" (open-output-string)))
           ) ;capture-write-error
      ) ;err
     ) ;
  (check (raw-string-write-error? err) => #t)
) ;let

;; =======================================
;; read-raw-string
;; =======================================



(check (read-raw-string (open-input-string "#\"\"a\"\"")) => "a")

(check (read-raw-string (open-input-string "#\"tag with space\"hello\"tag with space\"")
       ) ;read-raw-string
  =>
  "hello"
) ;check

(check (read-raw-string (open-input-string "#\"(())\"#\"\"a\"\"\"(())\""))
  =>
  "#\"\"a\"\""
) ;check

(check-catch 'raw-string-read-error (read-raw-string (open-input-string "#x")))

;; =======================================
;; read-raw-string-after-prefix
;; =======================================



(check (read-raw-string-after-prefix (open-input-string "\"a\"\"")) => "a")

(check (read-raw-string-after-prefix (open-input-string "tag with space\"hello\"tag with space\"")
       ) ;read-raw-string-after-prefix
  =>
  "hello"
) ;check

(check (read-raw-string-after-prefix (open-input-string "(())\"#\"\"a\"\"\"(())\""))
  =>
  "#\"\"a\"\""
) ;check

(check-catch 'raw-string-read-error
  (read-raw-string-after-prefix (open-input-string "unterminated"))
) ;check-catch

;; =======================================
;; can-delimit?
;; =======================================



(check (can-delimit? "abc" "") => #t)
(check (can-delimit? "\"" "") => #f)
(check (can-delimit? "\"\"" "") => #f)
(check (can-delimit? "\"\"" "-") => #t)
(check (can-delimit? "abc" "\"") => #f)
(check (can-delimit? "abc" "tag with space") => #t)
(check (can-delimit? "abc" "(())") => #t)

;; =======================================
;; generate-delimiter
;; =======================================



(let* ((payload "\"-\" \"--\" \"---\" \"----\"")
       (delimiter (generate-delimiter payload))
      ) ;
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

(let* ((payload "#\"\"a\"\" and \"quotes\"") (delimiter (generate-delimiter payload)))
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

;; =======================================
;; write-raw-string
;; =======================================



(let ((out (open-output-string)))
  (write-raw-string "hello" "" out)
  (check (get-output-string out) => "#\"\"hello\"\"")
) ;let

(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload))
       (out (open-output-string))
      ) ;
  (write-raw-string payload delimiter out)
  (check (read-raw-string (open-input-string (get-output-string out))) => payload)
) ;let*

(check-catch 'raw-string-write-error
  (write-raw-string "\"" "" (open-output-string))
) ;check-catch
