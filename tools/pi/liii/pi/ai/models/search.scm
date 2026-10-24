;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai models search)
  (import (scheme base) (scheme char) (liii string) (liii list))
  (export pi-split-search-tokens pi-fuzzy-match-model?)
  (begin

    (define (pi-split-search-tokens str)
      (if (not (string? str))
        '()
        (let ((len (string-length str)))
          (let loop
            ((i 0) (cur '()) (acc '()))
            (if (>= i len)
              (if (null? cur) (reverse acc) (reverse (cons (list->string (reverse cur)) acc)))
              (let ((ch (string-ref str i)))
                (if (or (char-whitespace? ch) (char=? ch #\/))
                  (if (null? cur)
                    (loop (+ i 1) '() acc)
                    (loop (+ i 1) '() (cons (list->string (reverse cur)) acc))
                  ) ;if
                  (loop (+ i 1) (cons ch cur) acc)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (%subsequence-match? query-str text-str)
      (let ((q (string-downcase query-str)) (t (string-downcase text-str)))
        (let ((q-len (string-length q)) (t-len (string-length t)))
          (let loop
            ((qi 0) (ti 0))
            (cond ((>= qi q-len) #t)
                  ((>= ti t-len) #f)
                  ((char=? (string-ref q qi) (string-ref t ti)) (loop (+ qi 1) (+ ti 1)))
                  (else (loop qi (+ ti 1)))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (pi-fuzzy-match-model? search-query text)
      (if (or (not search-query) (not (string? search-query)))
        #t
        (let ((tokens (pi-split-search-tokens search-query)))
          (or (null? tokens) (every (lambda (tok) (%subsequence-match? tok text)) tokens))
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
