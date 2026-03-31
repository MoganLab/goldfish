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

(define-library (liii golddoc-function)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-library)
          (liii path)
          (liii string)
  ) ;import
  (export exported-name->test-stem
          function-doc-path
  ) ;export
  (begin

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
                       (loop (+ index 2) (cons "-to-" parts))
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
                       (loop (+ index 1) (cons "-slash-" parts))
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
        (and candidate
             (path-file? candidate)
             candidate
        ) ;and
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
