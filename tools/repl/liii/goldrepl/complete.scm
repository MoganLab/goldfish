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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldrepl complete)
  (import (scheme base) (scheme char) (liii base) (liii list) (liii string))
  (export *repl-symbols* %repl-symbol-char? %repl-update-symbol-cache!
    %repl-symbol-name? %repl-completions %repl-word-at %repl-replace-word
    %repl-string-common-prefix %repl-common-prefix
  ) ;export
  (begin

    (define *repl-symbols* '())

    (define (%repl-symbol-char? ch)
      (or (char-alphabetic? ch) (string-contains "!$%&*/:<=>?^_~+-." (string ch)))
    ) ;define

    (define (%repl-entry-doc val)
      (catch #t
        (lambda () (if (procedure? val) (or (documentation val) "") ""))
        (lambda (tag info) "")
      ) ;catch
    ) ;define

    (define (%repl-update-symbol-cache!)
      (set! *repl-symbols*
        (let loop
          ((rest (let->list (rootlet))) (acc '()))
          (if (null? rest)
            acc
            (let* ((entry (car rest))
                   (name (if (pair? entry) (car entry) entry))
                   (val (if (pair? entry) (cdr entry) #f))
                   (doc (%repl-entry-doc val))
                  ) ;
              (loop (cdr rest)
                (if (symbol? name) (cons (cons (symbol->string name) doc) acc) acc)
              ) ;loop
            ) ;let*
          ) ;if
        ) ;let
      ) ;set!
    ) ;define

    (define (%repl-symbol-name? token)
      (let loop
        ((rest *repl-symbols*))
        (cond ((null? rest) #f)
              ((string=? (caar rest) token) #t)
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%repl-completions prefix)
      (let ((n (string-length prefix)))
        (let loop
          ((rest *repl-symbols*) (acc '()))
          (cond ((null? rest) (reverse acc))
                ((and (>= (string-length (caar rest)) n)
                   (string=? (substring (caar rest) 0 n) prefix)
                 ) ;and
                 (loop (cdr rest) (cons (car rest) acc))
                ) ;
                (else (loop (cdr rest) acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%repl-word-at text col)
      (let ((chars (string->list text)) (k (min col (string-length text))))
        (let loop
          ((i k) (acc '()))
          (if
            (and (> i 0) (%repl-symbol-char? (list-ref chars (- i 1))))
            (loop (- i 1) (cons (list-ref chars (- i 1)) acc))
            (list->string acc)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%repl-replace-word text col word replacement)
      (let* ((chars (string->list text))
             (k (min col (length chars)))
             (start
               (let loop
                 ((i k))
                 (if
                   (and (> i 0) (%repl-symbol-char? (list-ref chars (- i 1))))
                   (loop (- i 1))
                   i
                 ) ;if
               ) ;let
             ) ;start
            ) ;
        (string-append (list->string (take chars start))
          replacement
          (list->string (drop chars k))
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%repl-string-common-prefix a b)
      (let ((n (min (string-length a) (string-length b))))
        (let loop
          ((i 0))
          (if
            (or (>= i n) (not (char=? (string-ref a i) (string-ref b i))))
            (substring a 0 i)
            (loop (+ i 1))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%repl-common-prefix strs)
      (if (null? strs)
        ""
        (let loop
          ((prefix (car strs)) (rest (cdr strs)))
          (if (null? rest)
            prefix
            (loop (%repl-string-common-prefix prefix (car rest)) (cdr rest))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
