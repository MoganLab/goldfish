;; Acknowledgements
;;
;; This implementation is based on:
;;
;; 1. The idea deindent from guile-raw-strings by François Joulaud
;;    https://codeberg.org/avalenn/guile-raw-strings
;;    SPDX-License-Identifier: 0BSD
;;
;; 2. The deindentation follows C# raw string literal rules
;;    https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/raw-string

(define-library (liii raw-string)
  (import (srfi srfi-267) (srfi srfi-1) (srfi srfi-13) (liii error))
  (export raw-string-read-error?
    raw-string-write-error?
    read-raw-string
    read-raw-string-after-prefix
    can-delimit?
    generate-delimiter
    write-raw-string
    deindent
    &-
  ) ;export
  (begin
    (define (string-split-lines str)
      (let ((len (string-length str)))
        (let loop
          ((start 0) (result '()))
          (let ((nl-pos (string-index str #\newline start len)))
            (if (not nl-pos)
              (reverse (cons (substring str start len) result))
              (loop (+ nl-pos 1) (cons (substring str start nl-pos) result))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;; deindent / &- are compile-time macros: the reader has already turned
    ;; the multi-line raw-string literal into a single string datum, so the
    ;; indentation is computed at expand time and the result is a constant.
    ;; The expander evaluates procedural transformer bodies at expand time,
    ;; so the helper below can use the srfi-1/srfi-13 procedures directly.

    (define-syntax deindent-impl
      (lambda (stx)
        (let* ((str (cadr (syntax->datum stx))))
          (datum->syntax stx
            (let* ((lines (let loop ((start 1) (result '()))
                            (let ((nl-pos (string-index str #\newline start (string-length str))))
                              (if (not nl-pos)
                                (reverse (cons (substring str start (string-length str)) result))
                                (loop (+ nl-pos 1) (cons (substring str start nl-pos) result))))))
                   (closing-line (last lines))
                   (ref-indent (string-count closing-line #\space))
                   (content-lines (drop-right lines 1)))
              (string-join (map (lambda (line) (if (string-null? line) "" (substring line ref-indent)))
                             content-lines)
                "\n"))))))

    (define-syntax deindent
      (syntax-rules ()
        [(_ s) (deindent-impl s)]))

    (define-syntax &-
      (syntax-rules ()
        [(_ s) (deindent-impl s)]))
  ) ;begin
) ;define-library
