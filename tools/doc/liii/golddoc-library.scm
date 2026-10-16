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

(define-library (liii golddoc-library)
  (import (scheme base) (liii golddoc-args) (liii path) (liii string))
  (export excluded-test-group? find-visible-library-root
    find-tests-root-for-load-root library-doc-path
  ) ;export
  (begin

    (define (trim-trailing-separators value)
      (let loop
        ((current value))
        (if (and (> (string-length current) 1)
              (or (string-ends? current "/") (string-ends? current "\\"))
            ) ;and
          (loop (substring current 0 (- (string-length current) 1)))
          current
        ) ;if
      ) ;let
    ) ;define

    (define (excluded-test-group? group)
      (or (string=? group "goldfish"))
    ) ;define

    (define (parts->source-file-segments parts)
      (let loop
        ((rem parts) (acc '()))
        (cond ((null? rem) acc)
              ((null? (cdr rem)) (append acc (list (string-append (car rem) ".scm"))))
              (else
                (loop (cdr rem) (append acc (list (car rem))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (parts->test-file-segments parts)
      (let loop
        ((rem parts) (acc '()))
        (cond ((null? rem) acc)
              ((null? (cdr rem)) (append acc (list (string-append (car rem) "-test.scm"))))
              (else
                (loop (cdr rem) (append acc (list (car rem))))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (find-visible-library-root query)
      (let ((parts (parse-library-query query)))
        (if (not parts)
          #f
          (let ((rel-segments (parts->source-file-segments parts)))
            (let loop
              ((roots *load-path*))
              (if (null? roots)
                #f
                (let ((load-root (car roots)))
                  (if (and (string? load-root) (path-file? (apply path-join load-root rel-segments)))
                    load-root
                    (loop (cdr roots))
                  ) ;if
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-tests-root-for-load-root load-root)
      (if (not (string? load-root))
        #f
        (let* ((normalized-load-root (trim-trailing-separators load-root))
               (normalized-parent (trim-trailing-separators (path->string (path-parent normalized-load-root)))
               ) ;normalized-parent
               (direct-root (path->string (path-join normalized-load-root "tests")))
               (sibling-root (path->string (path-join normalized-parent "tests")))
              ) ;
          (cond ((path-dir? direct-root) direct-root)
                ((path-dir? sibling-root) sibling-root)
                (else #f)
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (library-doc-path query)
      (let* ((parts (parse-library-query query))
             (group (and parts (car parts)))
             (load-root (and parts (not (excluded-test-group? group)) (find-visible-library-root query))
             ) ;load-root
             (tests-root (and load-root (find-tests-root-for-load-root load-root)))
             (candidate
               (and tests-root
                 (path->string (apply path-join tests-root (parts->test-file-segments parts)))
               ) ;and
             ) ;candidate
            ) ;
        (if (and candidate (path-file? candidate)) candidate #f)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
