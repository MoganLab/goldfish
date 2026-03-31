;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii golddoc-index)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-library)
          (liii njson)
          (liii path)
          (liii string)
  ) ;import
  (export index-entry->library-query
          find-function-index-paths
          load-function-index
          visible-function-names
          visible-libraries-for-function
  ) ;export
  (begin

    (define (append-unique-string strings value)
      (if (or (not (string? value))
              (member value strings))
          strings
          (append strings (list value))
      ) ;if
    ) ;define

    (define (string-list-only values)
      (let loop ((remaining values)
                 (result '()))
        (if (null? remaining)
            result
            (let ((value (car remaining)))
              (if (string? value)
                  (loop (cdr remaining) (append result (list value)))
                  (loop (cdr remaining) result)
              ) ;if
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (normalize-object-alist value)
      (if (equal? value '(()))
          '()
          value
      ) ;if
    ) ;define

    (define (index-entry->library-query entry)
      (if (not (string? entry))
          #f
          (let* ((trimmed (string-trim entry))
                 (body (and (string-starts? trimmed "(")
                            (string-ends? trimmed ")")
                            (substring trimmed 1 (- (string-length trimmed) 1))))
                 (parts (and body (string-split body " "))))
            (if (and parts
                     (= (length parts) 2)
                     (not (string-null? (car parts)))
                     (not (string-null? (cadr parts))))
                (string-append (car parts) "/" (cadr parts))
                #f
            ) ;if
          ) ;let*
      ) ;if
    ) ;define

    (define (find-function-index-paths)
      (let loop ((roots *load-path*)
                 (tests-roots '())
                 (index-paths '()))
        (if (null? roots)
            index-paths
            (let* ((load-root (car roots))
                   (tests-root (and (string? load-root)
                                    (find-tests-root-for-load-root load-root)))
                   (already-seen (and tests-root (member tests-root tests-roots)))
                   (index-path (and tests-root
                                    (not already-seen)
                                    (path->string (path-join tests-root "function-library-index.json")))))
              (loop (cdr roots)
                    (if (and tests-root (not already-seen))
                        (append tests-roots (list tests-root))
                        tests-roots
                    ) ;if
                    (if (and index-path (path-file? index-path))
                        (append index-paths (list index-path))
                        index-paths
                    ) ;if
              ) ;loop
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (merge-function-index-entries current entries)
      (let ((merged current))
        (for-each
          (lambda (entry)
            (let* ((function-name (car entry))
                   (libraries (string-list-only (cdr entry)))
                   (cell (assoc function-name merged)))
              (if cell
                  (for-each
                    (lambda (library-entry)
                      (set-cdr! cell (append-unique-string (cdr cell) library-entry))
                    ) ;lambda
                    libraries
                  ) ;for-each
                  (set! merged
                        (append merged
                                (list (cons function-name libraries)))
                  ) ;set!
              ) ;if
            ) ;let*
          ) ;lambda
          entries
        ) ;for-each
        merged
      ) ;let
    ) ;define

    (define (load-function-index)
      (let loop ((index-paths (find-function-index-paths))
                 (merged '()))
        (if (null? index-paths)
            merged
            (let ((entries
                   (let-njson ((root (file->njson (car index-paths))))
                     (normalize-object-alist (njson-object->alist root))
                   ) ;let-njson
                   ))
              (loop (cdr index-paths)
                    (merge-function-index-entries merged entries))
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (visible-library-query? library-query)
      (let* ((parts (parse-library-query library-query))
             (group (and parts (car parts))))
        (and parts
             (not (excluded-test-group? group))
             (find-visible-library-root library-query)
             library-query
        ) ;and
      ) ;let*
    ) ;define

    (define (visible-function-names)
      (let loop ((entries (load-function-index))
                 (visible '()))
        (if (null? entries)
            visible
            (let* ((entry (car entries))
                   (function-name (car entry))
                   (library-entries (cdr entry))
                   (has-visible-library?
                     (let visible-loop ((remaining library-entries))
                       (and (not (null? remaining))
                            (or (let ((library-query (index-entry->library-query (car remaining))))
                                  (and library-query
                                       (visible-library-query? library-query))
                                ) ;let
                                (visible-loop (cdr remaining))
                            ) ;or
                       ) ;and
                     ) ;let
                   ))
              (loop (cdr entries)
                    (if (and has-visible-library?
                             (not (member function-name visible)))
                        (append visible (list function-name))
                        visible
                    ) ;if
              ) ;loop
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (visible-libraries-for-function function-name)
      (let* ((entry (assoc function-name (load-function-index)))
             (library-entries (if entry (cdr entry) '())))
        (let loop ((remaining library-entries)
                   (visible '()))
          (if (null? remaining)
              visible
              (let* ((library-query (index-entry->library-query (car remaining)))
                     (visible-library (and library-query
                                           (visible-library-query? library-query))))
                (loop (cdr remaining)
                      (if visible-library
                          (append-unique-string visible visible-library)
                          visible
                      ) ;if
                ) ;loop
              ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
