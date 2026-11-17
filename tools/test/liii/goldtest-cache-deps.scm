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

(define-library (liii goldtest-cache-deps)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii os)
    (liii path)
    (liii list)
    (liii string)
    (liii sort)
    (liii gfproject config)
  ) ;import
  (export extract-file-imports resolve-import-path find-test-dependencies
    test-no-cache-directive?
  ) ;export
  (begin

    (define (%norm-path p)
      (if (and (string? p) (os-windows?)) (string-replace p "\\" "/") p)
    ) ;define

    (define (%substring-index str pat)
      (let ((slen (string-length str)) (plen (string-length pat)))
        (if (< slen plen)
          #f
          (let loop
            ((i 0))
            (cond ((> (+ i plen) slen) #f)
                  ((string=? (substring str i (+ i plen)) pat) i)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 解包 R7RS 导入集修饰符（only, except, prefix, rename）
    (define (%unwrap-import-set set)
      (cond ((not (pair? set)) #f)
            ((memq (car set) '(only except prefix rename)) (%unwrap-import-set (cadr set)))
            ((and (pair? set) (symbol? (car set))) set)
            (else #f)
      ) ;cond
    ) ;define

    ;; 从单条顶层 AST 提取导入库名列表
    (define (%form-imported-libs form)
      (cond
       ((and (pair? form) (eq? (car form) 'import))
        (filter (lambda (x) x) (map %unwrap-import-set (cdr form)))
       ) ;
       ((and (pair? form) (eq? (car form) 'define-library))
        (let loop
          ((clauses (cddr form)) (acc '()))
          (cond ((null? clauses) (reverse acc))
                ((and (pair? (car clauses)) (eq? (caar clauses) 'import))
                 (loop (cdr clauses)
                   (append
                     (reverse (filter (lambda (x) x) (map %unwrap-import-set (cdar clauses))))
                     acc
                   ) ;append
                 ) ;loop
                ) ;
                (else (loop (cdr clauses) acc))
          ) ;cond
        ) ;let
       ) ;
       (else '())
      ) ;cond
    ) ;define

    ;; 从文件中提取所有 (import ...) 库名列表
    (define (extract-file-imports file-path)
      (if (not (file-exists? file-path))
        '()
        (catch #t
          (lambda ()
            (call-with-input-file file-path
              (lambda (p)
                (let loop
                  ((f (read p)) (acc '()))
                  (if (eof-object? f)
                    (reverse acc)
                    (loop (read p) (append (reverse (%form-imported-libs f)) acc))
                  ) ;if
                ) ;let
              ) ;lambda
            ) ;call-with-input-file
          ) ;lambda
          (lambda args '())
        ) ;catch
      ) ;if
    ) ;define

    (define (%lib-name->rel-path lib-name)
      (string-append (string-join (map symbol->string lib-name) "/") ".scm")
    ) ;define

    (define (%join-cand base rel)
      (cond ((or (string=? base "") (string=? base ".")) rel)
            ((string-ends? base "/") (string-append base rel))
            (else (string-append base "/" rel))
      ) ;cond
    ) ;define

    (define (%first-existing-path cands)
      (let loop
        ((rest cands))
        (if (null? rest)
          #f
          (let ((p (%norm-path (car rest))))
            (if (file-exists? p) p (loop (cdr rest)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 从当前文件路径推导所属板块根目录（如 tools/pi/、demo/）
    (define (%current-file-root current-file)
      (if (not (string? current-file))
        #f
        (let ((norm (%norm-path current-file)))
          (cond
           ((string-starts? norm "tools/")
            (let ((parts (string-split norm "/")))
              (if (>= (length parts) 2) (string-append (car parts) "/" (cadr parts)) #f)
            ) ;let
           ) ;
           ((string-starts? norm "demo/") "demo")
           (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 缓存静态搜索根目录（gfproject-domains-list 与 gf_lib 在进程内不变）
    (define *cached-base-roots* #f)

    (define (%base-search-roots)
      (unless *cached-base-roots*
        (let* ((dom-roots
                 (map
                   (lambda (d) (string-trim-right (cdr (assq 'root d)) #\/))
                   (gfproject-domains-list)
                 ) ;map
               ) ;dom-roots
               (gf-lib (gfproject-get-gf-lib))
               (gf-parent
                 (if gf-lib (path->string (path-parent (path gf-lib))) #f)
               ) ;gf-parent
               (gf-roots (if gf-lib
                           (list gf-lib
                             (string-append gf-lib "/r7rs")
                             (string-append gf-lib "/library")
                             (string-append gf-lib "/bignum")
                             (string-append gf-parent "/r7rs")
                             (string-append gf-parent "/library")
                             (string-append gf-parent "/bignum")
                           ) ;list
                           '()
                         ) ;if
               ) ;gf-roots
               (raw (append (list ".") dom-roots gf-roots))
              ) ;
          (set! *cached-base-roots*
            (filter
              (lambda (r) (and (string? r) (path-dir? (path r))))
              raw
            ) ;filter
          ) ;set!
        ) ;let*
      ) ;unless
      *cached-base-roots*
    ) ;define

    ;; 收集全局搜索路径（镜像 src/goldfish.hpp 的 load-path 顺序）
    (define (%search-roots current-file)
      (let ((cur-root (%current-file-root current-file)) (base (%base-search-roots)))
        (if (and cur-root (path-dir? (path cur-root))) (cons cur-root base) base)
      ) ;let
    ) ;define

    ;; 按真实加载路径将库名解析为 .scm 源文件路径（若为 C 内建库则返回 #f）
    (define (resolve-import-path lib-name current-file)
      (let ((rel (%lib-name->rel-path lib-name)) (roots (%search-roots current-file)))
        (%first-existing-path (map (lambda (r) (%join-cand r rel)) roots))
      ) ;let
    ) ;define

    ;; 定位启动时隐式加载的 R7RS 引导文件
    (define (%find-boot-files)
      (let* ((gf-lib (gfproject-get-gf-lib))
             (gf-parent
               (if gf-lib (path->string (path-parent (path gf-lib))) #f)
             ) ;gf-parent
             (bases
               (filter string?
                 (list "."
                   gf-lib
                   (and gf-lib (string-append gf-lib "/r7rs"))
                   (and gf-parent (string-append gf-parent "/r7rs"))
                 ) ;list
               ) ;filter
             ) ;bases
             (find-one
               (lambda (rel)
                 (%first-existing-path
                   (append (list (string-append "r7rs/scheme/" rel))
                     (map (lambda (b) (%join-cand b (string-append "scheme/" rel))) bases)
                   ) ;append
                 ) ;%first-existing-path
               ) ;lambda
             ) ;find-one
             (boot (find-one "boot.scm"))
             (syn (find-one "syntax-rules.scm"))
            ) ;
        (filter string? (list boot syn))
      ) ;let*
    ) ;define

    ;; 递归收集目录下所有普通文件
    (define (%collect-dir-files dir)
      (if (not (path-dir? (path dir)))
        '()
        (let loop
          ((entries (vector->list (path-list (path dir)))) (acc '()))
          (if (null? entries)
            acc
            (let* ((entry (car entries))
                   (full
                     (%norm-path (path->string (path-join (path dir) entry)))
                   ) ;full
                  ) ;
              (cond ((string-starts? entry ".") (loop (cdr entries) acc))
                    ((path-dir? (path full))
                     (loop (cdr entries) (append (%collect-dir-files full) acc))
                    ) ;
                    ((path-file? (path full)) (loop (cdr entries) (cons full acc)))
                    (else (loop (cdr entries) acc))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 推导测试所属测试根下的 resources/ 目录（如 tests/resources、library/tests/resources）
    (define (%find-resources-dir test-file)
      (let ((norm (%norm-path test-file)))
        (cond ((string-starts? norm "tests/") "tests/resources")
              ((string-contains norm "/tests/")
               (let ((idx (%substring-index norm "/tests/")))
                 (if idx (string-append (substring norm 0 idx) "/tests/resources") #f)
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 读取文件头部注释中的 @test-no-cache / @test-depends: 指令
    (define (%read-header-lines file-path)
      (if (not (file-exists? file-path))
        '()
        (catch #t
          (lambda ()
            (call-with-input-file file-path
              (lambda (p)
                (let loop
                  ((i 0) (acc '()))
                  (if (>= i 40)
                    (reverse acc)
                    (let ((line (read-line p)))
                      (if (eof-object? line) (reverse acc) (loop (+ i 1) (cons line acc)))
                    ) ;let
                  ) ;if
                ) ;let
              ) ;lambda
            ) ;call-with-input-file
          ) ;lambda
          (lambda args '())
        ) ;catch
      ) ;if
    ) ;define

    (define (test-no-cache-directive? file-path)
      (let loop
        ((lines (%read-header-lines file-path)))
        (cond ((null? lines) #f)
              ((and (string-contains (car lines) ";")
                 (string-contains (car lines) "@test-no-cache")
               ) ;and
               #t
              ) ;
              (else (loop (cdr lines)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%extract-test-depends file-path)
      (let loop
        ((lines (%read-header-lines file-path)) (acc '()))
        (if (null? lines)
          (reverse acc)
          (let* ((line (car lines))
                 (idx (and (string-contains line ";") (%substring-index line "@test-depends:")))
                ) ;
            (if idx
              (let ((spec (string-trim (substring line (+ idx 14)))))
                (if (> (string-length spec) 0)
                  (loop (cdr lines) (cons spec acc))
                  (loop (cdr lines) acc)
                ) ;if
              ) ;let
              (loop (cdr lines) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 递归解析测试文件、引导文件、resources/ 及 @test-depends 全部依赖，按字母序返回
    (define (find-test-dependencies test-file)
      (let ((visited '()))
        (define (add-dep! p)
          (if (and p (not (member p visited string=?)))
            (begin
              (set! visited (cons p visited))
              #t
            ) ;begin
            #f
          ) ;if
        ) ;define
        (define (visit-scm file)
          (for-each
            (lambda (lib)
              (let ((dep (resolve-import-path lib file)))
                (when (and dep (add-dep! dep))
                  (visit-scm dep)
                ) ;when
              ) ;let
            ) ;lambda
            (extract-file-imports file)
          ) ;for-each
        ) ;define
        (when (file-exists? test-file)
          (visit-scm test-file)
          (for-each (lambda (bf) (when (add-dep! bf) (visit-scm bf))) (%find-boot-files))
          (let ((res-dir (%find-resources-dir test-file)))
            (when res-dir
              (for-each add-dep! (%collect-dir-files res-dir))
            ) ;when
          ) ;let
          (for-each
            (lambda (spec)
              (cond ((path-dir? (path spec)) (for-each add-dep! (%collect-dir-files spec)))
                    ((file-exists? spec) (add-dep! (%norm-path spec)))
              ) ;cond
            ) ;lambda
            (%extract-test-depends test-file)
          ) ;for-each
        ) ;when
        (list-sort string<? visited)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
