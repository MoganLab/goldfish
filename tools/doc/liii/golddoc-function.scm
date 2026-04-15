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

(define-library (liii golddoc-function)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-library)
          (liii os)
          (liii path)
          (liii string)
  ) ;import
  (export exported-name->test-stem
          library-documented-functions
          function-doc-path
  ) ;export
  (begin

    (define golddoc-section-headings
      '("语法" "参数" "返回值" "描述" "说明" "注意" "错误处理" "示例")
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

    (define (file-documents-function? path exported-name)
      (let loop ((lines (string-split (path-read-text path) "\n")))
        (and (not (null? lines))
             (let ((body (comment-line->body (car lines)))
                   (candidate (comment-line->candidate (car lines))))
               (or (and candidate
                        (string=? candidate exported-name))
                   (and body
                        (or (string=? body (string-append "(" exported-name ")"))
                            (string-starts? body (string-append "(" exported-name " ")))
                   ) ;and
                   (loop (cdr lines))
               ) ;or
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (test-file-documented-function-name path)
      (let* ((lines (string-split (path-read-text path) "\n"))
             (file-stem (string-remove-suffix (path-stem path) "-test")))
        (let loop ((remaining lines))
          (and (not (null? remaining))
               (let ((candidate (comment-line->candidate (car remaining))))
                 (if (and candidate
                          (or (string=? (exported-name->test-stem candidate) file-stem)
                              (file-documents-function? path candidate)))
                     candidate
                     (loop (cdr remaining))
                 ) ;if
               ) ;let
          ) ;and
        ) ;let
      ) ;let*
    ) ;define

    (define (find-function-doc-by-scan library-dir exported-name)
      (if (not (path-dir? library-dir))
          #f
          (let loop ((entries (vector->list (listdir library-dir))))
            (and (not (null? entries))
                 (let* ((entry-name (car entries))
                        (entry-path (path->string (path-join library-dir entry-name))))
                   (if (and (path-file? entry-path)
                            (string-ends? entry-name "-test.scm")
                            (file-documents-function? entry-path exported-name))
                       entry-path
                       (loop (cdr entries))
                   ) ;if
                 ) ;let*
            ) ;and
          ) ;let
      ) ;if
    ) ;define

    (define (library-documented-functions library-query)
      (let* ((parts (parse-library-query library-query))
             (group (and parts (car parts)))
             (load-root (and parts
                             (not (excluded-test-group? group))
                             (find-visible-library-root library-query)))
             (tests-root (and load-root
                              (find-tests-root-for-load-root load-root)))
             (library-dir (and tests-root
                               (path->string (path-join tests-root group (cdr parts))))))
        (if (not (and library-dir (path-dir? library-dir)))
            '()
            (let loop ((entries (vector->list (listdir library-dir)))
                       (functions '()))
              (if (null? entries)
                  functions
                  (let ((entry-name (car entries)))
                    (if (string-ends? entry-name "-test.scm")
                        (let* ((test-file (path->string (path-join library-dir entry-name)))
                               (function-name (test-file-documented-function-name test-file)))
                          (loop (cdr entries)
                                (if (and function-name
                                         (not (member function-name functions)))
                                    (append functions (list function-name))
                                    functions
                                ) ;if
                          ) ;loop
                        ) ;let*
                        (loop (cdr entries) functions)
                    ) ;if
                  ) ;let
              ) ;if
            ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pure-operator->stem name)
      (cond
        ((string=? name "+") "plus")
        ((string=? name "-") "minus")
        ((string=? name "*") "star")
        ((string=? name "/") "slash")
        ((string=? name "=") "eq")
        ((string=? name "<") "lt")
        ((string=? name "<=") "le")
        ((string=? name ">") "gt")
        ((string=? name ">=") "ge")
        (else #f)
      ) ;cond
    ) ;define

    (define (string-starts-at? str index fragment)
      (let ((fragment-length (string-length fragment))
            (string-length* (string-length str)))
        (and (<= (+ index fragment-length) string-length*)
             (string=? (substring str index (+ index fragment-length)) fragment)
        ) ;and
      ) ;let
    ) ;define

    (define (exported-name->test-stem name)
      (let ((pure-operator (pure-operator->stem name)))
        (if pure-operator
            pure-operator
            (let ((name-length (string-length name)))
              (let loop ((index 0)
                         (parts '()))
                (if (>= index name-length)
                    (apply string-append (reverse parts))
                    (cond
                      ((string-starts-at? name index "->")
                       (loop (+ index 2)
                             (cons (if (= (+ index 2) name-length)
                                       "-to"
                                       "-to-")
                                   parts)
                       ) ;loop
                      ) ;
                      ((string-starts-at? name index ">=")
                       (loop (+ index 2) (cons "-ge" parts))
                      ) ;
                      ((string-starts-at? name index "<=")
                       (loop (+ index 2) (cons "-le" parts))
                      ) ;
                      ((char=? (string-ref name index) #\?)
                       (loop (+ index 1) (cons "-p" parts))
                      ) ;
                      ((char=? (string-ref name index) #\!)
                       (loop (+ index 1) (cons "-bang" parts))
                      ) ;
                      ((char=? (string-ref name index) #\/)
                       (loop (+ index 1)
                             (cons (if (= (+ index 1) name-length)
                                       "-slash"
                                       "-slash-")
                                   parts)
                       ) ;loop
                      ) ;
                      ((char=? (string-ref name index) #\*)
                       (loop (+ index 1) (cons "-star" parts))
                      ) ;
                      ((char=? (string-ref name index) #\=)
                       (loop (+ index 1) (cons "-eq" parts))
                      ) ;
                      ((char=? (string-ref name index) #\<)
                       (loop (+ index 1) (cons "-lt" parts))
                      ) ;
                      ((char=? (string-ref name index) #\>)
                       (loop (+ index 1) (cons "-gt" parts))
                      ) ;
                      (else
                       (loop (+ index 1)
                             (cons (string (string-ref name index)) parts)
                       ) ;loop
                      ) ;else
                    ) ;cond
                ) ;if
              ) ;let
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (function-doc-path library-query exported-name)
      (let* ((parts (parse-library-query library-query))
             (group (and parts (car parts)))
             (library (and parts (cdr parts)))
             (load-root (and parts
                             (not (excluded-test-group? group))
                             (find-visible-library-root library-query))
             ) ;load-root
             (tests-root (and load-root
                              (find-tests-root-for-load-root load-root))
             ) ;tests-root
             (library-dir (and tests-root
                               (path->string (path-join tests-root group library)))
             ) ;library-dir
             (candidate (and tests-root
                             (path->string
                               (path-join tests-root
                                          group
                                          library
                                          (string-append (exported-name->test-stem exported-name)
                                                         "-test.scm"))
                                          ) ;string-append
                               ) ;path-join
                             ) ;path->string
             ) ;candidate
        (cond
          ((and candidate (path-file? candidate))
           candidate
          ) ;
          (library-dir
           (find-function-doc-by-scan library-dir exported-name)
          ) ;
          (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
