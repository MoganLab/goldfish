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

(define-library (liii goldcheck unit-test)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii gfproject config)
    (liii goldcheck path)
  ) ;import
  (export check-unit-test check-unit-test-dirs %standard-sections)
  (begin

    (define %standard-sections
      '("语法" "参数" "返回值" "说明" "描述" "注意" "错误处理")
    ) ;define

    (define (divider-string? str)
      (let ((s (string-trim str)))
        (and (> (string-length s) 1)
          (let loop
            ((i 0))
            (if (>= i (string-length s))
              #t
              (if (char=? (string-ref s i) #\-) (loop (+ i 1)) #f)
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (comment-body line)
      (let ((trimmed (string-trim line)))
        (if (string-starts? trimmed ";")
          (let loop
            ((i 0))
            (if (and (< i (string-length trimmed)) (char=? (string-ref trimmed i) #\;))
              (loop (+ i 1))
              (string-trim (substring trimmed i (string-length trimmed)))
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (read-all-lines file-path)
      (let ((port (open-input-file file-path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ((acc '()))
              (let ((line (read-line port)))
                (if (eof-object? line) (reverse acc) (loop (cons line acc)))
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (find-first-non-empty-index lines)
      (let loop
        ((i 0))
        (cond ((>= i (length lines)) #f)
              ((not (string-null? (string-trim (list-ref lines i)))) i)
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-last-non-empty-index lines)
      (let loop
        ((i (- (length lines) 1)))
        (cond ((< i 0) #f)
              ((not (string-null? (string-trim (list-ref lines i)))) i)
              (else (loop (- i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (extract-doc-entries lines start-idx)
      (let loop
        ((i start-idx) (acc '()))
        (if (>= i (length lines))
          (reverse acc)
          (let* ((line (list-ref lines i)) (s (string-trim line)))
            (cond
             ((string-starts? s ";;") (loop (+ i 1) (cons (cons (+ i 1) line) acc)))
             ((string-null? s) (loop (+ i 1) acc))
             (else (reverse acc))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-doc-elements doc-entries rel-path)
      (let ((fn-name #f)
            (fn-line #f)
            (has-desc #f)
            (has-syntax #f)
            (has-params #f)
            (has-ret #f)
            (first-sec-seen #f)
            (viols '())
            (n (length doc-entries))
           ) ;
        (let loop
          ((k 0))
          (when (< k n)
            (let* ((entry (list-ref doc-entries k))
                   (l-num (car entry))
                   (body (comment-body (cdr entry)))
                   (next-body
                     (if (< (+ k 1) n) (comment-body (cdr (list-ref doc-entries (+ k 1)))) #f)
                   ) ;next-body
                   (has-div (and next-body (divider-string? next-body)))
                  ) ;
              (when (and body (not (string-null? body)))
                (cond
                 ((not fn-name)
                  (if (or (member body %standard-sections) (divider-string? body))
                    (set! viols
                      (cons (list 'unit-test rel-path l-num "缺少函数名声明") viols)
                    ) ;set!
                    (begin
                      (set! fn-name body)
                      (set! fn-line l-num)
                    ) ;begin
                  ) ;if
                 ) ;
                 ((string=? body "示例")
                  (set! first-sec-seen #t)
                  (set! viols
                    (cons (list 'unit-test
                            rel-path
                            l-num
                            "不应包含【示例】段落，因为文档下方的单元测试用例即为可执行示例"
                          ) ;list
                      viols
                    ) ;cons
                  ) ;set!
                 ) ;
                 ((member body %standard-sections)
                  (set! first-sec-seen #t)
                  (unless has-div
                    (set! viols
                      (cons (list 'unit-test
                              rel-path
                              l-num
                              (string-append "小节标题【" body "】下方必须紧跟 '----' 分割线")
                            ) ;list
                        viols
                      ) ;cons
                    ) ;set!
                  ) ;unless
                  (when (string=? body "语法")
                    (set! has-syntax #t)
                  ) ;when
                  (when (string=? body "参数")
                    (set! has-params #t)
                  ) ;when
                  (when (string=? body "返回值")
                    (set! has-ret #t)
                  ) ;when
                 ) ;
                 (has-div (set! first-sec-seen #t)
                   (set! viols
                     (cons (list 'unit-test
                             rel-path
                             l-num
                             (string-append "未知的小节标题【" body "】")
                           ) ;list
                       viols
                     ) ;cons
                   ) ;set!
                 ) ;has-div
                 (else
                   (when (and (not first-sec-seen) (not (divider-string? body)))
                     (set! has-desc #t)
                   ) ;when
                 ) ;else
                ) ;cond
              ) ;when
              (loop (+ k 1))
            ) ;let*
          ) ;when
        ) ;let
        (when (and fn-name (not has-desc))
          (set! viols
            (cons (list 'unit-test rel-path fn-line "缺少函数简要描述") viols)
          ) ;set!
        ) ;when
        (when (not has-syntax)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【语法】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (when (not has-params)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【参数】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (when (not has-ret)
          (set! viols
            (cons (list 'unit-test rel-path (if fn-line fn-line 1) "缺少【返回值】段落")
              viols
            ) ;cons
          ) ;set!
        ) ;when
        (reverse viols)
      ) ;let
    ) ;define

    (define (check-report-tail lines rel-path)
      (let ((last-idx (find-last-non-empty-index lines)))
        (cond
         ((not last-idx)
          (list (list 'unit-test
                  rel-path
                  (length lines)
                  "(check-report) 必须位于文件最后一行"
                ) ;list
          ) ;list
         ) ;
         ((not (string=? (string-trim (list-ref lines last-idx)) "(check-report)"))
          (list (list 'unit-test
                  rel-path
                  (+ last-idx 1)
                  "(check-report) 必须位于文件最后一行"
                ) ;list
          ) ;list
         ) ;
         ((or (<= last-idx 0)
            (not
              (string-null? (string-trim (list-ref lines (- last-idx 1))))
            ) ;not
          ) ;or
          (list (list 'unit-test
                  rel-path
                  (+ last-idx 1)
                  "(check-report) 上方必须保留一行空行"
                ) ;list
          ) ;list
         ) ;
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (parent->lib-name rel-parent)
      (let* ((norm (normalize-sep rel-parent))
             (clean (if (string-starts-with? norm "tests/")
                      (substring norm 6 (string-length norm))
                      norm
                    ) ;if
             ) ;clean
            ) ;
        (string-append "(" (string-join (string-split clean "/") " ") ")")
      ) ;let*
    ) ;define

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

    (define (exported-name->test-stem name)
      (let ((pure-operator (pure-operator->stem name)))
        (if pure-operator
          pure-operator
          (let ((name-length (string-length name)))
            (let loop
              ((index 0) (parts '()))
              (if (>= index name-length)
                (apply string-append (reverse parts))
                (cond
                 ((string-starts-at? name index "->")
                  (loop (+ index 2) (cons (if (= (+ index 2) name-length) "-to" "-to-") parts))
                 ) ;
                 ((string-starts-at? name index ">=") (loop (+ index 2) (cons "-ge" parts)))
                 ((string-starts-at? name index "<=") (loop (+ index 2) (cons "-le" parts)))
                 ((char=? (string-ref name index) #\?) (loop (+ index 1) (cons "-p" parts)))
                 ((char=? (string-ref name index) #\!) (loop (+ index 1) (cons "-bang" parts)))
                 ((char=? (string-ref name index) #\/)
                  (loop (+ index 1)
                    (cons (if (= (+ index 1) name-length) "-slash" "-slash-") parts)
                  ) ;loop
                 ) ;
                 ((char=? (string-ref name index) #\*) (loop (+ index 1) (cons "-star" parts)))
                 ((char=? (string-ref name index) #\=) (loop (+ index 1) (cons "-eq" parts)))
                 ((char=? (string-ref name index) #\<) (loop (+ index 1) (cons "-lt" parts)))
                 ((char=? (string-ref name index) #\>) (loop (+ index 1) (cons "-gt" parts)))
                 (else
                   (loop (+ index 1) (cons (string (string-ref name index)) parts))
                 ) ;else
                ) ;cond
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

    (define (find-library-source-for-test-dir parent)
      (let* ((rel-parent (relativize-path parent)) (norm (normalize-sep rel-parent)))
        (cond
         ((string-starts-with? norm "tools/")
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
         ) ;
         ((string-starts-with? norm "tests/")
          (let* ((parts (string-split norm "/"))
                 (subpath (string-join (cdr parts) "/"))
                 (candidate (resolve-repo-file (string-append "goldfish/" subpath ".scm")))
                ) ;
            (if (path-file? (path candidate)) candidate #f)
          ) ;let*
         ) ;
         (else
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
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (extract-library-exports source-path)
      (let ((port (open-input-file source-path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ()
              (let ((expr (catch #t (lambda () (read port)) (lambda (k e) (eof-object)))))
                (cond ((eof-object? expr) '())
                      ((and (pair? expr) (eq? (car expr) 'define-library))
                       (let ((decls (cddr expr)))
                         (let export-loop
                           ((rem decls) (exports '()))
                           (if (null? rem)
                             (reverse exports)
                             (let ((decl (car rem)))
                               (if (and (pair? decl) (eq? (car decl) 'export))
                                 (let item-loop
                                   ((items (cdr decl)) (acc exports))
                                   (if (null? items)
                                     (export-loop (cdr rem) acc)
                                     (let ((item (car items)))
                                       (cond
                                        ((symbol? item) (item-loop (cdr items) (cons (symbol->string item) acc)))
                                        ((and (pair? item) (eq? (car item) 'rename) (= (length item) 3))
                                         (let ((ext (caddr item)))
                                           (item-loop (cdr items) (cons (symbol->string ext) acc))
                                         ) ;let
                                        ) ;
                                        (else (item-loop (cdr items) acc))
                                       ) ;cond
                                     ) ;let
                                   ) ;if
                                 ) ;let
                                 (export-loop (cdr rem) exports)
                               ) ;if
                             ) ;let
                           ) ;if
                         ) ;let
                       ) ;let
                      ) ;
                      (else (loop))
                ) ;cond
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (check-unit-test-dirs files)
      "检查所有测试文件所属的目录是否包含 README.md，同级是否存在废弃单测试文件，以及导出函数是否均有测试文件。"
      (let loop
        ((rem files) (seen '()) (viols '()))
        (if (null? rem)
          (reverse viols)
          (let* ((f (car rem))
                 (is-test (string-ends? f "-test.scm"))
                 (parent (if is-test (path->string (path-parent f)) #f))
                ) ;
            (if (or (not is-test) (member parent seen string=?))
              (loop (cdr rem) seen viols)
              (let* ((norm-parent (normalize-sep parent))
                     (rel-parent (relativize-path parent))
                     (readme (path-join parent "README.md"))
                     (legacy-path-str (string-append norm-parent "-test.scm"))
                     (legacy-rel (string-append rel-parent "-test.scm"))
                     (readme-rel (string-append rel-parent "/README.md"))
                     (cur-viols '())
                    ) ;
                (when (path-file? (path legacy-path-str))
                  (let ((lib-name (parent->lib-name rel-parent)))
                    (set! cur-viols
                      (cons (list 'unit-test
                              legacy-rel
                              1
                              (string-append lib-name " 的文档，现在是 "
                                readme-rel " ，" legacy-rel
                                " 这个方案已经废弃"
                              ) ;string-append
                            ) ;list
                        cur-viols
                      ) ;cons
                    ) ;set!
                  ) ;let
                ) ;when
                (unless (path-file? readme)
                  (set! cur-viols
                    (cons (list 'unit-test
                            (relativize-path (path->string readme))
                            1
                            "缺少库导览文档 README.md"
                          ) ;list
                      cur-viols
                    ) ;cons
                  ) ;set!
                ) ;unless
                (let ((source-path (find-library-source-for-test-dir parent)))
                  (when source-path
                    (let ((exported-names (extract-library-exports source-path)))
                      (for-each
                        (lambda (name)
                          (let* ((stem (exported-name->test-stem name))
                                 (test-filename (string-append stem "-test.scm"))
                                 (test-file-path (path-join parent test-filename))
                                 (expected-rel (string-append rel-parent "/" test-filename))
                                ) ;
                            (unless (path-file? (path (path->string test-file-path)))
                              (set! cur-viols
                                (cons (list 'unit-test
                                        expected-rel
                                        1
                                        (string-append "缺少导出函数 " name " 的单元测试")
                                      ) ;list
                                  cur-viols
                                ) ;cons
                              ) ;set!
                            ) ;unless
                          ) ;let*
                        ) ;lambda
                        exported-names
                      ) ;for-each
                    ) ;let
                  ) ;when
                ) ;let
                (loop (cdr rem) (cons parent seen) (append cur-viols viols))
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (check-unit-test-file-content file-path rel-path)
      (let ((lines (read-all-lines file-path)))
        (if (null? lines)
          (list (list 'unit-test rel-path 1 "空测试文件"))
          (let ((first-idx (find-first-non-empty-index lines)))
            (if
              (or (not first-idx)
                (not (string-starts? (string-trim (list-ref lines first-idx)) ";;"))
              ) ;or
              (list (list 'unit-test
                      rel-path
                      (if first-idx (+ first-idx 1) 1)
                      "文档必须置于文件最顶部（在 import 之前）"
                    ) ;list
              ) ;list
              (append (check-doc-elements (extract-doc-entries lines first-idx) rel-path)
                (check-report-tail lines rel-path)
              ) ;append
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (check-unit-test file-path)
      "检查单元测试文件结构规范：文档置顶、包含函数名/描述/语法/参数/返回值、末尾为(check-report)且上方有空行。"
      (let* ((rel-path (relativize-path file-path)) (norm-path (normalize-sep file-path)))
        (if (string-ends? norm-path "-test.scm")
          (let* ((dir-path (substring norm-path 0 (- (string-length norm-path) 9)))
                 (dir-rel (substring rel-path 0 (- (string-length rel-path) 9)))
                ) ;
            (if (path-dir? (path dir-path))
              (let* ((lib-name (parent->lib-name dir-rel))
                     (readme-rel (string-append dir-rel "/README.md"))
                    ) ;
                (list (list 'unit-test
                        rel-path
                        1
                        (string-append lib-name " 的文档，现在是 "
                          readme-rel " ，" rel-path " 这个方案已经废弃"
                        ) ;string-append
                      ) ;list
                ) ;list
              ) ;let*
              (check-unit-test-file-content file-path rel-path)
            ) ;if
          ) ;let*
          (check-unit-test-file-content file-path rel-path)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
