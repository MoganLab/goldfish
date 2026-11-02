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

(define-library (liii goldcheck unit-test source reexport)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck unit-test source parse)
  ) ;import
  (export find-base-for-source-path find-defining-library-test-dir
    find-imported-export-for-stem check-reexported-tests
    check-reexported-test-file set-exported-name-to-test-stem-fn!
    set-find-library-source-fn!
  ) ;export
  (begin

    (define (find-base-for-source-path source-path)
      (let* ((actual (resolve-repo-file source-path)) (norm (normalize-sep actual)))
        (let loop
          ((pos 0) (last-found #f))
          (let ((idx (string-index-of norm "/goldfish/" pos)))
            (if idx
              (loop (+ idx 1) idx)
              (if last-found
                (substring norm 0 last-found)
                (let ((repo (gfproject-root)))
                  (if repo repo "")
                ) ;let
              ) ;if
            ) ;if
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

    (define (candidate-source-path base-dir spec)
      (let ((rel-scm (lib-spec->relative-source-path spec)))
        (if (string=? base-dir "") rel-scm (path->string (path-join base-dir rel-scm)))
      ) ;let
    ) ;define

    (define (check-spec-defines-name? cand-path int-name)
      (and (path-file? (path cand-path))
        (let ((cand-exports (extract-library-exports cand-path))
              (cand-defs (extract-library-defined-exports cand-path))
             ) ;
          (or (member int-name cand-defs string=?)
            (member int-name cand-exports string=?)
          ) ;or
        ) ;let
      ) ;and
    ) ;define

    (define (find-defining-library-test-dir int-name source-path)
      (let ((struct (parse-library-structure source-path)))
        (if (not struct)
          #f
          (let ((imports (cdr (assq 'imports struct)))
                (base-dir (find-base-for-source-path source-path))
               ) ;
            (let loop
              ((rem imports))
              (if (null? rem)
                #f
                (let* ((spec (car rem)) (cand-path (candidate-source-path base-dir spec)))
                  (if (check-spec-defines-name? cand-path int-name)
                    (string-append "tests/" (string-join (map symbol->string spec) "/"))
                    (loop (cdr rem))
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-stem-in-exports exports stem)
      (let loop
        ((rem exports))
        (cond ((null? rem) #f)
              ((string=? stem (exported-name-to-stem-fn (car rem))) (car rem))
              (else (loop (cdr rem)))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-imported-export-for-stem stem imports base-dir)
      (let loop
        ((rem imports))
        (if (null? rem)
          #f
          (let* ((spec (car rem)) (cand-path (candidate-source-path base-dir spec)))
            (if (path-file? (path cand-path))
              (let* ((exports (extract-library-exports cand-path))
                     (found (find-stem-in-exports exports stem))
                    ) ;
                (if found
                  (cons found
                    (string-append "tests/" (string-join (map symbol->string spec) "/"))
                  ) ;cons
                  (loop (cdr rem))
                ) ;if
              ) ;let*
              (loop (cdr rem))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 延迟绑定 exported-name->test-stem 避免循环依赖
    (define *exported-name->test-stem-ref* #f)

    (define (set-exported-name-to-test-stem-fn! fn)
      (set! *exported-name->test-stem-ref* fn)
    ) ;define

    (define (exported-name-to-stem-fn name)
      (if *exported-name->test-stem-ref* (*exported-name->test-stem-ref* name) name)
    ) ;define

    (define (check-reexported-test-file-with-struct file-path rel-path stem
              struct source-path
            ) ;check-reexported-test-file-with-struct
      (let* ((defs (cdr (assq 'defs struct)))
             (imports (cdr (assq 'imports struct)))
             (base-dir (find-base-for-source-path source-path))
             (defined?
               (let dloop
                 ((rem defs))
                 (cond ((null? rem) #f)
                       ((string=? stem (exported-name-to-stem-fn (car rem))) #t)
                       (else (dloop (cdr rem)))
                 ) ;cond
               ) ;let
             ) ;defined?
            ) ;
        (if defined?
          '()
          (let ((found (find-imported-export-for-stem stem imports base-dir)))
            (if found
              (list (list 'unit-test
                      rel-path
                      1
                      (string-append "函数 "
                        (car found)
                        " 并非本库实现，应移动到 "
                        (cdr found)
                        "/ 目录"
                      ) ;string-append
                    ) ;list
              ) ;list
              (check-in-reexported-entries source-path stem rel-path)
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (check-in-reexported-entries source-path stem rel-path)
      (let* ((reexported (extract-library-reexported-entries source-path))
             (match
               (let rloop
                 ((rem reexported))
                 (cond ((null? rem) #f)
                       ((string=? stem (exported-name-to-stem-fn (caar rem))) (car rem))
                       (else (rloop (cdr rem)))
                 ) ;cond
               ) ;let
             ) ;match
            ) ;
        (if match
          (list (list 'unit-test
                  rel-path
                  1
                  (string-append "函数 "
                    (car match)
                    " 并非本库实现，不应在当前库提供单元测试"
                  ) ;string-append
                ) ;list
          ) ;list
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (check-reexported-test-file file-path)
      (let* ((rel-path (relativize-path file-path)) (norm-path (normalize-sep rel-path)))
        (if (not (string-ends? norm-path "-test.scm"))
          '()
          (let* ((parent (path->string (path-parent (path file-path))))
                 (source-path (find-lib-src-fn parent))
                ) ;
            (if (not source-path)
              '()
              (let ((struct (parse-library-structure source-path)))
                (if (not struct)
                  '()
                  (let* ((filename (path->string (path-name (path file-path))))
                         (len (string-length filename))
                         (stem (substring filename 0 (- len 9)))
                        ) ;
                    (check-reexported-test-file-with-struct file-path rel-path
                      stem struct source-path
                    ) ;check-reexported-test-file-with-struct
                  ) ;let*
                ) ;if
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define *find-library-source-ref* #f)

    (define (set-find-library-source-fn! fn)
      (set! *find-library-source-ref* fn)
    ) ;define

    (define (find-lib-src-fn parent)
      (if *find-library-source-ref* (*find-library-source-ref* parent) #f)
    ) ;define

    (define (make-reexport-violation ext-name target-dir expected-rel)
      (let ((msg (if target-dir
                   (string-append "函数 " ext-name
                     " 并非本库实现，应移动到 " target-dir "/ 目录"
                   ) ;string-append
                   (string-append "函数 "
                     ext-name
                     " 并非本库实现，不应在当前库提供单元测试"
                   ) ;string-append
                 ) ;if
            ) ;msg
           ) ;
        (list 'unit-test expected-rel 1 msg)
      ) ;let
    ) ;define

    (define (check-single-reexported-entry entry parent rel-parent source-path)
      (let* ((ext-name (car entry))
             (int-name (cdr entry))
             (stem (exported-name-to-stem-fn ext-name))
             (test-filename (string-append stem "-test.scm"))
             (test-path (path-join parent test-filename))
             (expected-rel (string-append rel-parent "/" test-filename))
            ) ;
        (if
          (path-file? (path (resolve-repo-file (path->string test-path))))
          (let ((target-dir (find-defining-library-test-dir int-name source-path)))
            (list (make-reexport-violation ext-name target-dir expected-rel))
          ) ;let
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (check-reexported-tests parent rel-parent)
      (let ((source-path (find-lib-src-fn parent)))
        (if (not source-path)
          '()
          (let ((reexported (extract-library-reexported-entries source-path)))
            (let loop
              ((rem reexported) (viols '()))
              (if (null? rem)
                (reverse viols)
                (let ((entry-viols (check-single-reexported-entry (car rem) parent rel-parent source-path)
                      ) ;entry-viols
                     ) ;
                  (loop (cdr rem) (append entry-viols viols))
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
