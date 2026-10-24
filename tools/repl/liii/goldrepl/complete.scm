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
    %repl-invalidate-symbol-cache! %repl-symbol-name? %repl-completions
    %repl-word-at %repl-replace-word %repl-string-common-prefix
    %repl-common-prefix
  ) ;export
  (begin

    (define *repl-symbols* '())
    (define *repl-symbol-cache-valid?* #f)

    (define (%repl-symbol-char? ch)
      (or (char-alphabetic? ch)
        (memv ch '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\+
                   #\- #\.))
      ) ;or
    ) ;define

    (define (%repl-maybe-update-symbol-cache!)
      (when (not *repl-symbol-cache-valid?*)
        (%repl-update-symbol-cache!)
      ) ;when
    ) ;define

    (define (%repl-invalidate-symbol-cache!)
      (set! *repl-symbol-cache-valid?* #f)
    ) ;define

    (define (%repl-entry-doc val)
      (catch #t
        (lambda () (if (procedure? val) (or (documentation val) "") ""))
        (lambda (tag info) "")
      ) ;catch
    ) ;define

    (define (%repl-update-symbol-cache!)
      (set! *repl-symbols*
        (fold
          (lambda (entry acc)
            (let* ((name (if (pair? entry) (car entry) entry))
                   (val (if (pair? entry) (cdr entry) #f))
                   (doc (%repl-entry-doc val))
                  ) ;
              (if (symbol? name) (cons (cons (symbol->string name) doc) acc) acc)
            ) ;let*
          ) ;lambda
          '()
          (let->list (rootlet))
        ) ;fold
      ) ;set!
      (set! *repl-symbol-cache-valid?* #t)
    ) ;define

    (define (%repl-symbol-name? token)
      (any (lambda (entry) (string=? (car entry) token)) *repl-symbols*)
    ) ;define

    (define (%repl-completions prefix)
      (%repl-maybe-update-symbol-cache!)
      (filter (lambda (entry) (string-starts? (car entry) prefix)) *repl-symbols*)
    ) ;define

    (define (%repl-word-at text col)
      (let ((k (min col (string-length text))))
        (let loop
          ((i k))
          (if
            (and (> i 0) (%repl-symbol-char? (string-ref text (- i 1))))
            (loop (- i 1))
            (substring text i k)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (%repl-replace-word text col word replacement)
      (let* ((k (min col (string-length text)))
             (start
               (let loop
                 ((i k))
                 (if
                   (and (> i 0) (%repl-symbol-char? (string-ref text (- i 1))))
                   (loop (- i 1))
                   i
                 ) ;if
               ) ;let
             ) ;start
            ) ;
        (string-append (substring text 0 start)
          replacement
          (substring text k (string-length text))
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
      (if (null? strs) "" (fold %repl-string-common-prefix (car strs) (cdr strs)))
    ) ;define

  ) ;begin
) ;define-library
