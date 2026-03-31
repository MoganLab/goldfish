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

(define-library (liii golddoc-index-build)
  (import (scheme base)
          (liii golddoc-function)
          (liii golddoc-library)
          (liii njson)
          (liii os)
          (liii path)
          (liii sort)
          (liii string)
  ) ;import
  (export build-function-indexes!)
  (begin

    (define golddoc-section-headings
      '("语法" "参数" "返回值" "描述" "说明" "注意" "错误处理" "示例")
    ) ;define

    (define (append-unique-string strings value)
      (if (or (not (string? value))
              (member value strings))
          strings
          (append strings (list value))
      ) ;if
    ) ;define

    (define (strip-leading-semicolons value)
      (let ((value-length (string-length value)))
        (let loop ((index 0))
          (if (or (>= index value-length)
                  (not (char=? (string-ref value index) #\;)))
              (substring value index value-length)
              (loop (+ index 1))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (divider-line? value)
      (let ((value-length (string-length value)))
        (and (> value-length 1)
             (let loop ((index 0))
               (if (>= index value-length)
                   #t
                   (and (char=? (string-ref value index) #\-)
                        (loop (+ index 1))
                   ) ;and
               ) ;if
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (comment-line->body line)
      (let ((trimmed (string-trim line)))
        (and (string-starts? trimmed ";")
             (let ((body (string-trim (strip-leading-semicolons trimmed))))
               (and (not (string-null? body))
                    body
               ) ;and
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (comment-line->candidate line)
      (let ((body (comment-line->body line)))
        (and body
             (not (member body golddoc-section-headings))
             (not (divider-line? body))
             (not (string-starts? body "Copyright"))
             (not (string-starts? body "Licensed under"))
             (not (string-starts? body "http://"))
             (not (string-starts? body "添加 tools/"))
             (let ((normalized (if (string-ends? body "函数测试")
                                   (string-trim-right
                                     (substring body
                                                0
                                                (- (string-length body)
                                                   (string-length "函数测试")))
                                   ) ;string-trim-right
                                   body
                               ) ;if
                   ))
               (and (not (string-null? normalized))
                    normalized
               ) ;and
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (syntax-documents-candidate? lines candidate)
      (let loop ((remaining lines))
        (and (not (null? remaining))
             (let ((body (comment-line->body (car remaining))))
               (or (and body
                        (or (string=? body (string-append "(" candidate ")"))
                            (string-starts? body (string-append "(" candidate " ")))
                   ) ;and
                   (loop (cdr remaining))
               ) ;or
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (supported-test-group? group)
      (not (excluded-test-group? group))
    ) ;define

    (define (find-buildable-tests-roots)
      (let loop ((roots *load-path*)
                 (tests-roots '()))
        (if (null? roots)
            tests-roots
            (let* ((load-root (car roots))
                   (tests-root (and (string? load-root)
                                    (find-tests-root-for-load-root load-root))))
              (loop (cdr roots)
                    (if (and tests-root
                             (not (member tests-root tests-roots)))
                        (append tests-roots (list tests-root))
                        tests-roots
                    ) ;if
              ) ;loop
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (sorted-dir-entries dir)
      (list-sort string<? (vector->list (listdir dir)))
    ) ;define

    (define (documented-function-name test-file)
      (let* ((lines (string-split (path-read-text test-file) "\n"))
             (file-stem (string-remove-suffix (path-stem test-file) "-test")))
        (let loop ((remaining lines))
          (and (not (null? remaining))
               (let ((candidate (comment-line->candidate (car remaining))))
                 (if (and candidate
                          (or (string=? (exported-name->test-stem candidate) file-stem)
                              (syntax-documents-candidate? lines candidate)))
                     candidate
                     (loop (cdr remaining))
                 ) ;if
               ) ;let
          ) ;and
        ) ;let
      ) ;let*
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

    (define (build-index-for-tests-root tests-root)
      (let ((index '()))
        (for-each
          (lambda (group-name)
            (let ((group-dir (path->string (path-join tests-root group-name))))
              (if (and (path-dir? group-dir)
                       (supported-test-group? group-name))
                  (for-each
                    (lambda (library-name)
                      (let ((library-dir (path->string (path-join group-dir library-name))))
                        (if (path-dir? library-dir)
                            (for-each
                              (lambda (entry-name)
                                (if (string-ends? entry-name "-test.scm")
                                    (let* ((test-file (path->string (path-join library-dir entry-name)))
                                           (function-name (documented-function-name test-file)))
                                      (if function-name
                                          (set! index
                                                (index-add! index
                                                            function-name
                                                            (string-append "(" group-name " " library-name ")"))
                                          ) ;set!
                                      ) ;if
                                    ) ;let*
                                ) ;if
                              ) ;lambda
                              (sorted-dir-entries library-dir)
                            ) ;for-each
                        ) ;if
                      ) ;let
                    ) ;lambda
                    (sorted-dir-entries group-dir)
                  ) ;for-each
              ) ;if
            ) ;let
          ) ;lambda
          (sorted-dir-entries tests-root)
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

    (define (build-function-index-at! tests-root)
      (let ((index-path (path->string (path-join tests-root "function-library-index.json"))))
        (let-njson ((index-json (json->njson (index->json-value (build-index-for-tests-root tests-root)))))
          (njson->file index-path index-json)
        ) ;let-njson
        index-path
      ) ;let
    ) ;define

    (define (build-function-indexes!)
      (let loop ((tests-roots (find-buildable-tests-roots))
                 (built-paths '()))
        (if (null? tests-roots)
            built-paths
            (loop (cdr tests-roots)
                  (append built-paths
                          (list (build-function-index-at! (car tests-roots))))
            ) ;loop
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
