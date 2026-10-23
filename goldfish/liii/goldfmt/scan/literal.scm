;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii goldfmt scan literal)
  (export rewrite-reader-literals needs-rewrite-reader-literals?
  ) ;export

  (import (liii base)
    (liii raw-string)
    (liii string)
  ) ;import

  (begin

    (define (write-to-string value)
      (let ((port (open-output-string)))
        (let-temporarily (((*s7* 'print-length) 9223372036854775807))
          (write value port)
        ) ;let-temporarily
        (get-output-string port)
      ) ;let
    ) ;define

    (define (fast-string-join strings)
      (let ((out (open-output-string)))
        (let loop
          ((rest strings))
          (if (null? rest)
            (get-output-string out)
            (begin
              (display (car rest) out)
              (loop (cdr rest))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (string-prefix-at? str start prefix)
      (let ((prefix-len (string-length prefix)) (str-len (string-length str)))
        (and (<= (+ start prefix-len) str-len)
          (let loop
            ((i 0))
            (or (>= i prefix-len)
              (and (char=? (string-ref str (+ start i)) (string-ref prefix i)) (loop (+ i 1)))
            ) ;or
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (find-raw-string-literal-end str body-start delimiter)
      (let ((needle (string-append "\"" delimiter "\"")) (str-len (string-length str)))
        (let loop
          ((i body-start))
          (cond
           ((> (+ i (string-length needle)) str-len) #f)
           ((string-prefix-at? str i needle) (+ i (string-length needle)))
           (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (read-raw-string-literal-value literal)
      (let ((value (read (open-input-string literal))))
        (if (string? value)
          value
          (error 'value-error "rewrite-raw-string-literals: expected raw string literal")
        ) ;if
      ) ;let
    ) ;define

    (define (reader-delimiter? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
        (char=? c #\()
        (char=? c #\))
        (char=? c #\[)
        (char=? c #\])
        (char=? c #\")
        (char=? c #\;)
        (char=? c #\')
        (char=? c #\`)
        (char=? c #\,)
      ) ;or
    ) ;define

    (define (find-char-literal-end source start)
      (let ((len (string-length source)))
        (if (>= start len)
          #f
          (let loop
            ((i (+ start 1)))
            (if (>= i len)
              len
              (let ((c (string-ref source i)))
                (if (reader-delimiter? c) i (loop (+ i 1)))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (rewrite-reader-literals source)
      (let ((len (string-length source)))
        (let loop
          ((i 0)
           (in-string #f)
           (escaped #f)
           (in-line-comment #f)
           (in-block-comment #f)
           (result '())
          ) ;
          (if (>= i len)
            (fast-string-join (reverse! result))
            (let ((c (string-ref source i))
                  (next-c (if (< (+ i 1) len) (string-ref source (+ i 1)) #\nul))
                 ) ;
              (cond
                (in-line-comment (loop (+ i 1) #f #f (not (char=? c #\newline)) #f (cons (string c) result))
                ) ;in-line-comment
                (in-block-comment
                  (if (and (char=? c #\|) (char=? next-c #\#))
                    (loop (+ i 2) #f #f #f #f (cons "|#" (cons (string c) result)))
                    (loop (+ i 1) #f #f #f #t (cons (string c) result))
                  ) ;if
                ) ;in-block-comment
                (in-string
                  (cond
                    (escaped (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                    ((char=? c #\\) (loop (+ i 1) #t #t #f #f (cons (string c) result)))
                    ((char=? c #\") (loop (+ i 1) #f #f #f #f (cons (string c) result)))
                    (else (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                  ) ;cond
                ) ;in-string
                ((and (char=? c #\;) (char=? next-c #\;))
                 (loop (+ i 2) #f #f #t #f (cons ";;" result))
                ) ;
                ((and (char=? c #\#) (char=? next-c #\|))
                 (loop (+ i 2) #f #f #f #t (cons "#|" result))
                ) ;
                ((char=? c #\") (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                ((and (char=? c #\#) (char=? next-c #\"))
                 (let ((delimiter-end (string-index source #\" (+ i 2))))
                   (if delimiter-end
                     (let* ((delimiter (substring source (+ i 2) delimiter-end))
                            (literal-end (find-raw-string-literal-end source (+ delimiter-end 1) delimiter))
                           ) ;
                       (if literal-end
                         (let* ((literal (substring source i literal-end))
                                (value (read-raw-string-literal-value literal))
                                (rewritten (string-append "(*raw-string* "
                                             (write-to-string literal)
                                             " "
                                             (write-to-string value)
                                             ")"
                                           ) ;string-append
                                ) ;rewritten
                               ) ;
                           (loop literal-end #f #f #f #f (cons rewritten result))
                         ) ;let*
                         (loop (+ i 1) #f #f #f #f (cons (string c) result))
                       ) ;if
                     ) ;let*
                     (loop (+ i 1) #f #f #f #f (cons (string c) result))
                   ) ;if
                 ) ;let
                ) ;
                ((and (char=? c #\#) (char=? next-c #\\))
                 (let ((literal-end (find-char-literal-end source (+ i 2))))
                   (if literal-end
                     (let* ((literal (substring source i literal-end))
                            (value (read (open-input-string literal)))
                            (rewritten (string-append "(*char-literal* " (write-to-string literal) " " literal ")")
                            ) ;rewritten
                           ) ;
                       (if (char? value)
                         (loop literal-end #f #f #f #f (cons rewritten result))
                         (loop (+ i 1) #f #f #f #f (cons (string c) result))
                       ) ;if
                     ) ;let*
                     (loop (+ i 1) #f #f #f #f (cons (string c) result))
                   ) ;if
                 ) ;let
                ) ;
                (else (loop (+ i 1) #f #f #f #f (cons (string c) result)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (needs-rewrite-reader-literals? str)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((>= i len) #f)
                ((and (char=? (string-ref str i) #\#)
                   (< (+ i 1) len)
                   (or (char=? (string-ref str (+ i 1)) #\") (char=? (string-ref str (+ i 1)) #\\))
                 ) ;and
                 #t
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
