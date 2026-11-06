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
    (liii os)
    (liii path)
    (liii string)
    (liii list)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck unit-test source parse)
    (liii goldcheck unit-test source reexport)
  ) ;import
  (export exported-name->test-stem find-library-source-for-test-dir
    extract-library-exports extract-library-defined-exports
    extract-library-reexported-entries check-missing-exported-tests
    check-reexported-tests check-reexported-test-file file-defines-library?
    find-test-dir-for-library-source hint-source-path?
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
       ((char=? (string-ref name index) #\:)
        (cons (if (= (+ index 1) name-length) "-colon" "-colon-") 1)
       ) ;
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

    (define (find-tools-library-source norm)
      (if (string-starts? norm "tools/")
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
      ;; 仓库分为四个板块：r7rs/（R7RS 标准库）、goldfish/（语言层）、
      ;; library/（标准库）、tools/（工具）。测试目录与源码目录的对应关系
      ;; 统一为「<板块>/tests/<板块内路径>」：
      ;;   r7rs/tests/scheme/base    <-> r7rs/scheme/base.scm
      ;;   library/tests/liii/path   <-> library/liii/path.scm
      ;;   tests/liii/foo            <-> goldfish/liii/foo.scm
      (let* ((parts (string-split norm "/"))
             (prefix (car parts))
             (rest (cdr parts))
             ;; 板块内测试目录形如 <板块>/tests/<子路径>，去掉中间的 tests 段
             (sub
               (if (and (pair? rest) (string=? (car rest) "tests")) (cdr rest) rest)
             ) ;sub
             (rel
               (cond ((string=? prefix "r7rs") (string-append "r7rs/" (string-join sub "/")))
                     ((string=? prefix "library") (string-append "library/" (string-join sub "/")))
                     ((string=? prefix "tests") (string-append "goldfish/" (string-join rest "/")))
                     (else #f)
               ) ;cond
             ) ;rel
            ) ;
        (if (not rel)
          #f
          (let ((candidate (resolve-repo-file (string-append rel ".scm"))))
            (if (path-file? (path candidate)) candidate #f)
          ) ;let
        ) ;if
      ) ;let*
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

    (define (port-defines-library? port)
      (let loop
        ()
        (let ((expr (catch #t (lambda () (read port)) (lambda (k e) (eof-object)))))
          (cond ((eof-object? expr) #f)
                ((and (pair? expr) (eq? (car expr) 'define-library)) #t)
                (else (loop))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (file-defines-library? file-path)
      (let ((actual-path (resolve-repo-file file-path)))
        (and (path-file? (path actual-path))
          (catch #t
            (lambda () (call-with-input-file actual-path port-defines-library?))
            (lambda (k e) #f)
          ) ;catch
        ) ;and
      ) ;let
    ) ;define

    (define (find-tools-test-dir norm)
      (if (string-starts? norm "tools/")
        (let ((parts (string-split norm "/")))
          (if (and (>= (length parts) 3)
                (not (member "tests" parts string=?))
                (not (member "hint" parts string=?))
                (string-ends? norm ".scm")
              ) ;and
            (let* ((tool (cadr parts))
                   (subparts (cddr parts))
                   (subpath-ext (string-join subparts "/"))
                   (subpath (substring subpath-ext 0 (- (string-length subpath-ext) 4)))
                  ) ;
              (resolve-repo-file (string-append "tools/" tool "/tests/" subpath))
            ) ;let*
            #f
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (hint-source-path? norm)
      ;; hint 目录（类型提示库）不参与库源码与测试目录的互推导。
      (or (string-starts? norm "hint/") (string-contains? norm "/hint/"))
    ) ;define

    (define (find-tests-test-dir norm)
      ;; 源码路径 -> 测试目录：与 find-tests-library-source 互为逆映射。
      (cond ((hint-source-path? norm) #f)
            ((and (string-starts? norm "goldfish/") (string-ends? norm ".scm"))
             (resolve-repo-file
               (string-append "tests/" (substring norm 9 (- (string-length norm) 4)))
             ) ;resolve-repo-file
            ) ;
            ((and (string-starts? norm "r7rs/") (string-ends? norm ".scm"))
             (resolve-repo-file
               (string-append "r7rs/tests/" (substring norm 5 (- (string-length norm) 4)))
             ) ;resolve-repo-file
            ) ;
            ((and (string-starts? norm "library/") (string-ends? norm ".scm"))
             (resolve-repo-file
               (string-append "library/tests/" (substring norm 8 (- (string-length norm) 4)))
             ) ;resolve-repo-file
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (find-fixture-test-dir src-path)
      (let ((norm-abs (normalize-sep (path->string (path src-path)))))
        (if (and (string-contains? norm-abs "/goldfish/") (string-ends? norm-abs ".scm"))
          (let* ((idx (string-index-of norm-abs "/goldfish/" 0))
                 (left (substring norm-abs 0 idx))
                 (right (substring norm-abs (+ idx 10) (string-length norm-abs)))
                 (subpath (substring right 0 (- (string-length right) 4)))
                ) ;
            (string-append left "/tests/" subpath)
          ) ;let*
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (find-test-dir-for-library-source src-path)
      (if (not (file-defines-library? src-path))
        #f
        (let* ((norm-abs (normalize-sep (path->string (path src-path))))
               (rel (relativize-path src-path))
               (norm (normalize-sep rel))
              ) ;
          (cond ((string-contains? norm-abs "/tests/") #f)
                ((find-tools-test-dir norm) => (lambda (p) p))
                ((find-tests-test-dir norm) => (lambda (p) p))
                ((find-fixture-test-dir src-path) => (lambda (p) p))
                (else #f)
          ) ;cond
        ) ;let*
      ) ;if
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
            (if
              (path-file? (path (resolve-repo-file (path->string test-path))))
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
          (missing-tests-in-names (extract-library-defined-exports source-path)
            parent
            rel-parent
          ) ;missing-tests-in-names
        ) ;if
      ) ;let
    ) ;define

    (set-exported-name-to-test-stem-fn! exported-name->test-stem)
    (set-find-library-source-fn! find-library-source-for-test-dir)
    (set-find-test-dir-fn! find-test-dir-for-library-source)

  ) ;begin
) ;define-library
