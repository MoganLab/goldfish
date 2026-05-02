(define-library (liii ascii)
  (export ascii-codepoint?
    ascii-bytevector?

    ascii-char?
    ascii-string?

    ascii-control?
    ascii-non-control?
    ascii-whitespace?
    ascii-space-or-tab?
    ascii-other-graphic?
    ascii-upper-case?
    ascii-lower-case?
    ascii-alphabetic?
    ascii-alphanumeric?
    ascii-numeric?

    ascii-digit-value
    ascii-upper-case-value
    ascii-lower-case-value
    ascii-nth-digit
    ascii-nth-upper-case
    ascii-nth-lower-case
    ascii-upcase
    ascii-downcase
    ascii-control->graphic
    ascii-graphic->control
    ascii-mirror-bracket

    ascii-ci=?
    ascii-ci<?
    ascii-ci>?
    ascii-ci<=?
    ascii-ci>=?

    ascii-string-ci=?
    ascii-string-ci<?
    ascii-string-ci>?
    ascii-string-ci<=?
    ascii-string-ci>=?

    ascii-left-paren?
    ascii-right-paren?
  ) ;export
  (import (srfi srfi-175)
    (scheme char))
  (begin

    (define (ascii-upcase x)
      (if (char? x)
        (char-upcase x)
        (or (ascii-lower-case-value x 65 26) x)
      ) ;if
    ) ;define

    (define (ascii-downcase x)
      (if (char? x)
        (char-downcase x)
        (or (ascii-upper-case-value x 97 26) x)
      ) ;if
    ) ;define

    (define (ascii-left-paren? x)
      (if (char? x) (char=? x #\() (and (integer? x) (= x 40)))
    ) ;define

    (define (ascii-right-paren? x)
      (if (char? x) (char=? x #\)) (and (integer? x) (= x 41)))
    ) ;define

  ) ;begin
) ;define-library
