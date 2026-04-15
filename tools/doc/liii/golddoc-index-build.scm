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

(define-library (liii golddoc-index-build)
  (import (scheme base)
          (scheme read)
          (liii golddoc-library)
          (liii njson)
          (liii os)
          (liii path)
          (liii sort)
          (liii string)
  ) ;import
  (export build-function-indexes!)
  (begin

    (define (append-unique-string strings value)
      (if (or (not (string? value))
              (member value strings))
          strings
          (append strings (list value))
      ) ;if
    ) ;define

    (define (supported-test-group? group)
      (not (excluded-test-group? group))
    ) ;define

    (define (find-buildable-index-targets)
      (let loop ((roots *load-path*)
                 (tests-roots '())
                 (targets '()))
        (if (null? roots)
            targets
            (let* ((load-root (car roots))
                   (tests-root (and (string? load-root)
                                    (find-tests-root-for-load-root load-root))))
              (if (and tests-root
                       (not (member tests-root tests-roots)))
                  (loop (cdr roots)
                        (append tests-roots (list tests-root))
                        (append targets (list (cons load-root tests-root)))
                  ) ;loop
                  (loop (cdr roots)
                        tests-roots
                        targets
                  ) ;loop
              ) ;if
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (sorted-dir-entries dir)
      (list-sort string<? (vector->list (listdir dir)))
    ) ;define

    (define (index-add! index function-name library-entry)
      (let ((cell (assoc function-name index)))
        (if cell
            (set-cdr! cell (append-unique-string (cdr cell) library-entry))
            (set! index (append index (list (cons function-name (list library-entry)))))
        ) ;if
        index
      ) ;let
    ) ;define

    (define (index-add-exported-functions index library-entry exported-functions)
      (let loop ((remaining exported-functions)
                 (result index))
        (if (null? remaining)
            result
            (loop (cdr remaining)
                  (index-add! result (car remaining) library-entry))
        ) ;if
      ) ;let
    ) ;define

    (define (sorted-index index)
      (map (lambda (entry)
             (cons (car entry)
                   (list-sort string<? (cdr entry)))
           ) ;lambda
           (list-sort (lambda (left right)
                        (string<? (car left) (car right)))
                      index)
      ) ;map
    ) ;define

    (define (library-form? form)
      (and (pair? form)
           (eq? (car form) 'define-library)
           (pair? (cdr form))
      ) ;and
    ) ;define

    (define (export-form? form)
      (and (pair? form)
           (eq? (car form) 'export)
      ) ;and
    ) ;define

    (define (library-name-part->string value)
      (cond
        ((symbol? value)
         (symbol->string value)
        ) ;
        ((number? value)
         (number->string value)
        ) ;
        (else
         #f
        ) ;else
      ) ;cond
    ) ;define

    (define (library-name->entry library-name)
      (if (not (and (list? library-name)
                    (= (length library-name) 2)))
          #f
          (let* ((group (library-name-part->string (car library-name)))
                 (library (library-name-part->string (cadr library-name))))
            (and group
                 library
                 (supported-test-group? group)
                 (string-append "(" group " " library ")")
            ) ;and
          ) ;let*
      ) ;if
    ) ;define

    (define (rename-export-spec? value)
      (and (pair? value)
           (eq? (car value) 'rename)
           (= (length value) 3)
      ) ;and
    ) ;define

    (define (export-spec->name spec)
      (cond
        ((symbol? spec)
         (symbol->string spec)
        ) ;
        ((rename-export-spec? spec)
         (library-name-part->string (caddr spec))
        ) ;
        (else
         #f
        ) ;else
      ) ;cond
    ) ;define

    (define (export-form-names form)
      (let loop ((remaining (cdr form))
                 (names '()))
        (if (null? remaining)
            names
            (let ((name (export-spec->name (car remaining))))
              (loop (cdr remaining)
                    (if name
                        (append-unique-string names name)
                        names
                    ) ;if
              ) ;loop
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (define-library-exported-names form)
      (let loop ((remaining (cddr form))
                 (names '()))
        (if (null? remaining)
            names
            (let ((declaration (car remaining)))
              (loop (cdr remaining)
                    (if (export-form? declaration)
                        (let export-loop ((exports (export-form-names declaration))
                                          (result names))
                          (if (null? exports)
                              result
                              (export-loop (cdr exports)
                                           (append-unique-string result (car exports)))
                          ) ;if
                        ) ;let
                        names
                    ) ;if
              ) ;loop
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (index-add-library-form index form)
      (if (not (library-form? form))
          index
          (let ((library-entry (library-name->entry (cadr form))))
            (if library-entry
                (index-add-exported-functions index
                                              library-entry
                                              (define-library-exported-names form))
                index
            ) ;if
          ) ;let
      ) ;if
    ) ;define

    (define (index-add-source-file index library-file)
      (call-with-input-file
        library-file
        (lambda (port)
          (let loop ((result index))
            (let ((form (read port)))
              (if (eof-object? form)
                  result
                  (loop (index-add-library-form result form))
              ) ;if
            ) ;let
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;define

    (define (build-index-for-load-root load-root)
      (let ((index '()))
        (for-each
          (lambda (group-name)
            (let ((group-dir (path->string (path-join load-root group-name))))
              (if (and (path-dir? group-dir)
                       (supported-test-group? group-name))
                  (for-each
                    (lambda (entry-name)
                      (let ((source-file (path->string (path-join group-dir entry-name))))
                        (if (and (path-file? source-file)
                                 (string-ends? entry-name ".scm"))
                            (set! index (index-add-source-file index source-file))
                        ) ;if
                      ) ;let
                    ) ;lambda
                    (sorted-dir-entries group-dir)
                  ) ;for-each
              ) ;if
            ) ;let
          ) ;lambda
          (sorted-dir-entries load-root)
        ) ;for-each
        (sorted-index index)
      ) ;let
    ) ;define

    (define (index->json-value index)
      (map (lambda (entry)
             (cons (car entry)
                   (list->vector (cdr entry)))
           ) ;lambda
           index
      ) ;map
    ) ;define

    (define (build-function-index-at! load-root tests-root)
      (let ((index-path (path->string (path-join tests-root "function-library-index.json"))))
        (let* ((raw-index (build-index-for-load-root load-root))
               (json-value (index->json-value raw-index))
               ;; 当索引为空时，使用空对象 '(()) 代替空列表
               (normalized-json-value (if (null? json-value) '(()) json-value)))
          (let-njson ((index-json (json->njson normalized-json-value)))
            (njson->file index-path index-json)
          ) ;let-njson
        ) ;let*
        index-path
      ) ;let
    ) ;define

    (define (build-function-indexes!)
      (let loop ((targets (find-buildable-index-targets))
                 (built-paths '()))
        (if (null? targets)
            built-paths
            (let* ((target (car targets))
                   (load-root (car target))
                   (tests-root (cdr target)))
              (loop (cdr targets)
                    (append built-paths
                            (list (build-function-index-at! load-root tests-root)))
              ) ;loop
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
