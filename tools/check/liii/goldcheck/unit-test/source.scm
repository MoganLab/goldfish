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

(define-library (liii goldcheck unit-test source)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii gfproject config)
    (liii goldcheck path)
  ) ;import
  (export exported-name->test-stem find-library-source-for-test-dir
    extract-library-exports check-missing-exported-tests
  ) ;export
  (begin

    (define (pure-operator->stem name)
      (cond ((string=? name "+") "plus")
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
            (string-length* (string-length str))
           ) ;
        (and (<= (+ index fragment-length) string-length*)
          (string=? (substring str index (+ index fragment-length)) fragment)
        ) ;and
      ) ;let
    ) ;define

    (define (stem-part-at name index name-length)
      (cond
       ((string-starts-at? name index "->")
        (cons (if (= (+ index 2) name-length) "-to" "-to-") 2)
       ) ;
       ((string-starts-at? name index ">=") (cons "-ge" 2))
       ((string-starts-at? name index "<=") (cons "-le" 2))
       ((char=? (string-ref name index) #\?) (cons "-p" 1))
       ((char=? (string-ref name index) #\!) (cons "-bang" 1))
       ((char=? (string-ref name index) #\/)
        (cons (if (= (+ index 1) name-length) "-slash" "-slash-") 1)
       ) ;
       ((char=? (string-ref name index) #\*) (cons "-star" 1))
       ((char=? (string-ref name index) #\=) (cons "-eq" 1))
       ((char=? (string-ref name index) #\<) (cons "-lt" 1))
       ((char=? (string-ref name index) #\>) (cons "-gt" 1))
       (else (cons (string (string-ref name index)) 1))
      ) ;cond
    ) ;define

    (define (exported-name->test-stem name)
      (let ((op (pure-operator->stem name)))
        (if op
          op
          (let ((len (string-length name)))
            (let loop
              ((i 0) (parts '()))
              (if (>= i len)
                (apply string-append (reverse parts))
                (let ((step (stem-part-at name i len)))
                  (loop (+ i (cdr step)) (cons (car step) parts))
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (resolve-repo-file rel)
      (let ((root (gfproject-root)))
        (if (and root (string? root) (not (string=? root "")))
          (path->string (path-join root rel))
          rel
        ) ;if
      ) ;let
    ) ;define

    (define (find-tools-library-source norm)
      (if (string-starts-with? norm "tools/")
        (let ((parts (string-split norm "/")))
          (if (and (>= (length parts) 4) (string=? (list-ref parts 2) "tests"))
            (let* ((tool (cadr parts))
                   (subpath (string-join (list-tail parts 3) "/"))
                   (candidate (resolve-repo-file (string-append "tools/" tool "/" subpath ".scm")))
                  ) ;
              (if (path-file? (path candidate)) candidate #f)
            ) ;let*
            #f
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (find-tests-library-source norm)
      (if (string-starts-with? norm "tests/")
        (let* ((parts (string-split norm "/"))
               (subpath (string-join (cdr parts) "/"))
               (candidate (resolve-repo-file (string-append "goldfish/" subpath ".scm")))
              ) ;
          (if (path-file? (path candidate)) candidate #f)
        ) ;let*
        #f
      ) ;if
    ) ;define

    (define (find-fixture-library-source parent)
      (let ((norm-abs (normalize-sep (path->string (path parent)))))
        (if (string-contains? norm-abs "/tests/")
          (let* ((idx (string-index-of norm-abs "/tests/" 0))
                 (left (substring norm-abs 0 idx))
                 (right (substring norm-abs (+ idx 7) (string-length norm-abs)))
                 (cand1 (string-append left "/goldfish/" right ".scm"))
                 (cand2 (string-append left "/" right ".scm"))
                ) ;
            (cond ((path-file? (path cand1)) cand1)
                  ((path-file? (path cand2)) cand2)
                  (else #f)
            ) ;cond
          ) ;let*
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (find-library-source-for-test-dir parent)
      (let* ((rel-parent (relativize-path parent)) (norm (normalize-sep rel-parent)))
        (or (find-tools-library-source norm)
          (find-tests-library-source norm)
          (find-fixture-library-source parent)
        ) ;or
      ) ;let*
    ) ;define

    (define (extract-decl-exports decl exports)
      (if (and (pair? decl) (eq? (car decl) 'export))
        (let loop
          ((items (cdr decl)) (acc exports))
          (if (null? items)
            acc
            (let ((item (car items)))
              (cond
               ((symbol? item) (loop (cdr items) (cons (symbol->string item) acc)))
               ((and (pair? item) (eq? (car item) 'rename) (= (length item) 3))
                (let ((ext (caddr item)))
                  (loop (cdr items) (cons (symbol->string ext) acc))
                ) ;let
               ) ;
               (else (loop (cdr items) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        exports
      ) ;if
    ) ;define

    (define (extract-exports-from-port port)
      (let loop
        ()
        (let ((expr (catch #t (lambda () (read port)) (lambda (k e) (eof-object)))))
          (cond ((eof-object? expr) '())
                ((and (pair? expr) (eq? (car expr) 'define-library))
                 (let decl-loop
                   ((rem (cddr expr)) (exports '()))
                   (if (null? rem)
                     (reverse exports)
                     (decl-loop (cdr rem) (extract-decl-exports (car rem) exports))
                   ) ;if
                 ) ;let
                ) ;
                (else (loop))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (extract-library-exports source-path)
      (call-with-input-file source-path extract-exports-from-port)
    ) ;define

    (define (make-missing-test-violation expected-rel name)
      (list 'unit-test
        expected-rel
        1
        (string-append "缺少导出函数 " name " 的单元测试")
      ) ;list
    ) ;define

    (define (missing-tests-in-names names parent rel-parent)
      (let loop
        ((rem names) (acc '()))
        (if (null? rem)
          (reverse acc)
          (let* ((name (car rem))
                 (stem (exported-name->test-stem name))
                 (test-filename (string-append stem "-test.scm"))
                 (test-path (path-join parent test-filename))
                 (expected-rel (string-append rel-parent "/" test-filename))
                ) ;
            (if (path-file? (path (path->string test-path)))
              (loop (cdr rem) acc)
              (loop (cdr rem) (cons (make-missing-test-violation expected-rel name) acc))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-missing-exported-tests parent rel-parent)
      (let ((source-path (find-library-source-for-test-dir parent)))
        (if (not source-path)
          '()
          (missing-tests-in-names (extract-library-exports source-path) parent rel-parent)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
