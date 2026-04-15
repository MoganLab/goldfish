(import (liii check) (srfi srfi-267))

(check-set-mode! 'report-failed)

(define (capture-read-error thunk)
  (guard (err ((raw-string-read-error? err) err)
           (else 'unexpected-error)
         ) ;err
    (thunk)
    'no-error
  ) ;guard
) ;define

(define (capture-write-error thunk)
  (guard (err ((raw-string-write-error? err) err)
           (else 'unexpected-error)
         ) ;err
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
(check #"tag with space"hello"tag with space"
  =>
  "hello"
) ;check
(check #"(())"#""a"""(())"
  =>
  "#\"\"a\"\""
) ;check

(let ((port (open-input-string "#\"\"\"\"\""))
     ) ;
  (check (read-raw-string port) => "")
  (check (read-char port) => #\")
) ;let
(check-false (raw-string-read-error? 'plain-symbol)
) ;check-false
(let ((err (capture-read-error (lambda ()
                                 (read-raw-string (open-input-string "#x")
                                 ) ;read-raw-string
                               ) ;lambda
           ) ;capture-read-error
      ) ;err
     ) ;
  (check (raw-string-read-error? err)
    =>
    #t
  ) ;check
) ;let
(check-false (raw-string-write-error? 'plain-symbol)
) ;check-false
(let ((err (capture-write-error (lambda ()
                                  (write-raw-string "\""
                                    ""
                                    (open-output-string)
                                  ) ;write-raw-string
                                ) ;lambda
           ) ;capture-write-error
      ) ;err
     ) ;
  (check (raw-string-write-error? err)
    =>
    #t
  ) ;check
) ;let

;; =======================================
;; read-raw-string
;; =======================================



(check (read-raw-string (open-input-string "#\"\"a\"\"")
       ) ;read-raw-string
  =>
  "a"
) ;check

(check (read-raw-string (open-input-string "#\"tag with space\"hello\"tag with space\""
                        ) ;open-input-string
       ) ;read-raw-string
  =>
  "hello"
) ;check

(check (read-raw-string (open-input-string "#\"(())\"#\"\"a\"\"\"(())\""
                        ) ;open-input-string
       ) ;read-raw-string
  =>
  "#\"\"a\"\""
) ;check

(check-catch 'raw-string-read-error
  (read-raw-string (open-input-string "#x")
  ) ;read-raw-string
) ;check-catch

;; =======================================
;; read-raw-string-after-prefix
;; =======================================



(check (read-raw-string-after-prefix (open-input-string "\"a\"\"")
       ) ;read-raw-string-after-prefix
  =>
  "a"
) ;check

(check (read-raw-string-after-prefix (open-input-string "tag with space\"hello\"tag with space\""
                                     ) ;open-input-string
       ) ;read-raw-string-after-prefix
  =>
  "hello"
) ;check

(check (read-raw-string-after-prefix (open-input-string "(())\"#\"\"a\"\"\"(())\""
                                     ) ;open-input-string
       ) ;read-raw-string-after-prefix
  =>
  "#\"\"a\"\""
) ;check

(check-catch 'raw-string-read-error
  (read-raw-string-after-prefix (open-input-string "unterminated")
  ) ;read-raw-string-after-prefix
) ;check-catch

;; =======================================
;; can-delimit?
;; =======================================



(check (can-delimit? "abc" "") => #t)
(check (can-delimit? "\"" "") => #f)
(check (can-delimit? "\"\"" "") => #f)
(check (can-delimit? "\"\"" "-") => #t)
(check (can-delimit? "abc" "\"") => #f)
(check (can-delimit? "abc" "tag with space")
  =>
  #t
) ;check
(check (can-delimit? "abc" "(())")
  =>
  #t
) ;check

;; =======================================
;; generate-delimiter
;; =======================================



(let* ((payload "\"-\" \"--\" \"---\" \"----\""
       ) ;payload
       (delimiter (generate-delimiter payload))
      ) ;
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter)
    =>
    #t
  ) ;check
) ;let*

(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload))
      ) ;
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter)
    =>
    #t
  ) ;check
) ;let*

;; =======================================
;; write-raw-string
;; =======================================



(let ((out (open-output-string)))
  (write-raw-string "hello" "" out)
  (check (get-output-string out)
    =>
    "#\"\"hello\"\""
  ) ;check
) ;let

(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload))
       (out (open-output-string))
      ) ;
  (write-raw-string payload delimiter out)
  (check (read-raw-string (open-input-string (get-output-string out)
                          ) ;open-input-string
         ) ;read-raw-string
    =>
    payload
  ) ;check
) ;let*

(check-catch 'raw-string-write-error
  (write-raw-string "\""
    ""
    (open-output-string)
  ) ;write-raw-string
) ;check-catch
