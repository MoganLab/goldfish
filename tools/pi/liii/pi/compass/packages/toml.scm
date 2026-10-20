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

(define-library (liii pi compass packages toml)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi compass types)
    (liii pi compass packages read)
  ) ;import
  (export pi-compass-detect-cargo-package pi-compass-detect-pyproject-package)
  (begin

    (define (%between-quotes s)
      (let ((start (string-index s #\")))
        (if (not start)
          #f
          (let ((end (string-index (substring s (+ start 1) (string-length s)) #\")))
            (if (not end) #f (substring s (+ start 1) (+ start 1 end)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%line-key-value line)
      (let ((eq (string-index line #\=)))
        (if (not eq)
          (values #f #f)
          (values (string-trim-both (substring line 0 eq))
            (string-trim-both (substring line (+ eq 1) (string-length line)))
          ) ;values
        ) ;if
      ) ;let
    ) ;define
    (define (%cargo-section line name version deps loop)
      (let ((inner (substring line 1 (- (string-length line) 1))))
        (if (string-starts? inner "dependencies.")
          (loop
            (list "dependencies"
              name
              version
              (cons (substring inner 13 (string-length inner)) deps)
            ) ;list
          ) ;loop
          (loop (list inner name version deps))
        ) ;if
      ) ;let
    ) ;define
    (define (%cargo-dep-line line section name version deps loop)
      (let-values (((k v) (%line-key-value line)))
        (if
          (and (string? k)
            (not (string-null? k))
            (not (member k '("name" "version" "edition" "authors")))
          ) ;and
          (loop (list section name version (cons k deps)))
          (loop (list section name version deps))
        ) ;if
      ) ;let-values
    ) ;define
    (define (%cargo-metadata-line line section name version deps loop)
      (let-values (((k v) (%line-key-value line)))
        (cond
         ((and (string? k) (string=? k "name") (not name))
          (loop (list section (%between-quotes v) version deps))
         ) ;
         ((and (string? k) (string=? k "version") (not version))
          (loop (list section name (%between-quotes v) deps))
         ) ;
         (else (loop (list section name version deps)))
        ) ;cond
      ) ;let-values
    ) ;define
    (define (pi-compass-detect-cargo-package cwd)
      (let ((text
              (pi-compass-read-text (path->string (path-join (path cwd) "Cargo.toml")))
            ) ;text
           ) ;
        (if (not text)
          #f
          (let loop
            ((lines (string-split text "\n"))
             (section "")
             (name #f)
             (version #f)
             (deps '())
            ) ;
            (if (null? lines)
              (pi-compass-package "cargo" (or name "") version (reverse deps))
              (let ((line (string-trim-both (car lines))))
                (define rest (cdr lines))
                (define next
                  (lambda (state)
                    (loop rest
                      (list-ref state 0)
                      (list-ref state 1)
                      (list-ref state 2)
                      (list-ref state 3)
                    ) ;loop
                  ) ;lambda
                ) ;define
                (cond ((and (string-starts? line "[") (string-ends? line "]"))
                       (%cargo-section line name version deps next)
                      ) ;
                      ((or (string=? section "dependencies") (string=? section "dev-dependencies"))
                       (%cargo-dep-line line section name version deps next)
                      ) ;
                      (else (%cargo-metadata-line line section name version deps next))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%quoted-strings line)
      (let loop
        ((start 0) (acc '()))
        (let ((i (string-index (substring line start (string-length line)) #\")))
          (if (not i)
            (reverse acc)
            (let* ((open (+ start i))
                   (rest-start (+ open 1))
                   (j (string-index (substring line rest-start (string-length line)) #\"))
                  ) ;
              (if (not j)
                (reverse acc)
                (let ((close (+ rest-start j)))
                  (loop (+ close 1) (cons (substring line rest-start close) acc))
                ) ;let
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (%dep-name spec)
      (let loop
        ((i 0))
        (if (>= i (string-length spec))
          spec
          (let ((ch (string-ref spec i)))
            (if (memv ch '(#\> #\< #\= #\! #\~ #\^ #\space #\; #\[))
              (string-trim-both (substring spec 0 i))
              (loop (+ i 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%pyproject-deps lines)
      (let loop
        ((rest lines) (collecting #f) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((line (car rest)) (trimmed (string-trim-both line)))
            (cond
              (collecting
                (let ((names (map %dep-name (%quoted-strings trimmed))))
                  (loop (cdr rest)
                    (not (string-contains? trimmed "]"))
                    (append (reverse names) acc)
                  ) ;loop
                ) ;let
              ) ;collecting
              ((or (string-starts? trimmed "dependencies =")
                 (string-starts? trimmed "dependencies=")
               ) ;or
               (let ((names (map %dep-name (%quoted-strings trimmed))))
                 (loop (cdr rest)
                   (not (string-contains? trimmed "]"))
                   (append (reverse names) acc)
                 ) ;loop
               ) ;let
              ) ;
              (else (loop (cdr rest) #f acc))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (pi-compass-detect-pyproject-package cwd)
      (let ((text
              (pi-compass-read-text (path->string (path-join (path cwd) "pyproject.toml")))
            ) ;text
           ) ;
        (if (not text)
          #f
          (let* ((lines (string-split text "\n")) (name #f) (version #f))
            (for-each
              (lambda (line)
                (let ((trimmed (string-trim-both line)))
                  (let-values (((k v) (%line-key-value trimmed)))
                    (cond ((and (string? k) (string=? k "name") (not name))
                           (set! name (%between-quotes v))
                          ) ;
                          ((and (string? k) (string=? k "version") (not version))
                           (set! version (%between-quotes v))
                          ) ;
                          (else #f)
                    ) ;cond
                  ) ;let-values
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            (pi-compass-package (if (string-contains? text "[tool.poetry]") "poetry" "pip")
              (or name "")
              version
              (%pyproject-deps lines)
            ) ;pi-compass-package
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
