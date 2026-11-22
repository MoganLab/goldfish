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
    (liii hash-table)
    (liii gfproject config)
    (liii goldtest-project)
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
      (cond ((not (pair? form)) '())
            ((eq? (car form) 'import)
             (filter (lambda (x) x) (map %unwrap-import-set (cdr form)))
            ) ;
            ((eq? (car form) 'define-library)
             (apply append (map %form-imported-libs (cddr form)))
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define *imports-cache* (make-hash-table))
    (define *resolve-cache* (make-hash-table))
    (define *resources-cache* (make-hash-table))
    (define *boot-deps-cache* #f)
    (define *cached-base-roots* #f)

    (define (%read-file-imports file-path)
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
    ) ;define

    ;; 从文件中提取所有 (import ...) 库名列表（进程内按路径缓存）
    (define (extract-file-imports file-path)
      (cond ((not (file-exists? file-path)) '())
            ((hash-table-contains? *imports-cache* file-path)
             (hash-table-ref *imports-cache* file-path)
            ) ;
            (else
              (let ((res (%read-file-imports file-path)))
                (hash-table-set! *imports-cache* file-path res)
                res
              ) ;let
            ) ;else
      ) ;cond
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

    (define (%base-search-roots)
      (unless *cached-base-roots*
        (let* ((dom-roots (%project-domain-search-roots))
               (gf-lib (gfproject-get-gf-lib))
               (gf-roots
                 (if gf-lib
                   (map (lambda (s) (string-append gf-lib s))
                     '("" "/r7rs" "/library" "/bignum" "/cmark" "/tui"
                       "/../r7rs" "/../library" "/../bignum" "/../cmark"
                       "/../tui")
                   ) ;map
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

    (define (resolve-import-path lib-name current-file)
      (let* ((cur-root (%current-file-root current-file))
             (cache-key (cons cur-root lib-name))
            ) ;
        (if (hash-table-contains? *resolve-cache* cache-key)
          (hash-table-ref *resolve-cache* cache-key)
          (let* ((rel (%lib-name->rel-path lib-name))
                 (roots (%search-roots current-file))
                 (res
                   (%first-existing-path (map (lambda (r) (%join-cand r rel)) roots))
                 ) ;res
                ) ;
            (hash-table-set! *resolve-cache* cache-key res)
            res
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; 定位启动时隐式加载的 R7RS 引导文件
    (define (%find-boot-files)
      (let* ((gf-lib (gfproject-get-gf-lib))
             (bases
               (filter string?
                 (list "."
                   gf-lib
                   (and gf-lib (string-append gf-lib "/r7rs"))
                   (and gf-lib (string-append gf-lib "/../r7rs"))
                 ) ;list
               ) ;filter
             ) ;bases
            ) ;
        (filter string?
          (map
            (lambda (rel)
              (%first-existing-path
                (cons (string-append "r7rs/scheme/" rel)
                  (map (lambda (b) (%join-cand b (string-append "scheme/" rel))) bases)
                ) ;cons
              ) ;%first-existing-path
            ) ;lambda
            '("boot.scm" "syntax-rules.scm")
          ) ;map
        ) ;filter
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
      (any (lambda (line)
             (and (string-contains line ";") (string-contains line "@test-no-cache"))
           ) ;lambda
        (%read-header-lines file-path)
      ) ;any
    ) ;define

    (define (%extract-test-depends file-path)
      (filter
        (lambda (s) (and (string? s) (> (string-length s) 0)))
        (map
          (lambda (line)
            (let ((idx (and (string-contains line ";") (%substring-index line "@test-depends:")))
                 ) ;
              (and idx (string-trim (substring line (+ idx 14))))
            ) ;let
          ) ;lambda
          (%read-header-lines file-path)
        ) ;map
      ) ;filter
    ) ;define

    (define (%cached-dir-files dir)
      (if (hash-table-contains? *resources-cache* dir)
        (hash-table-ref *resources-cache* dir)
        (let ((files (%collect-dir-files dir)))
          (hash-table-set! *resources-cache* dir files)
          files
        ) ;let
      ) ;if
    ) ;define

    (define (%visit-scm-into! visited file)
      (for-each
        (lambda (lib)
          (let ((dep (resolve-import-path lib file)))
            (when (and dep (not (hash-table-contains? visited dep)))
              (hash-table-set! visited dep #t)
              (%visit-scm-into! visited dep)
            ) ;when
          ) ;let
        ) ;lambda
        (extract-file-imports file)
      ) ;for-each
    ) ;define

    (define (%add-dep! visited p)
      (when (and p (not (hash-table-contains? visited p)))
        (hash-table-set! visited p #t)
      ) ;when
    ) ;define

    (define (%get-boot-deps)
      (unless *boot-deps-cache*
        (let ((visited (make-hash-table)))
          (for-each (lambda (s) (%add-dep! visited s) (%visit-scm-into! visited s))
            (%find-boot-files)
          ) ;for-each
          (set! *boot-deps-cache* (hash-table-keys visited))
        ) ;let
      ) ;unless
      *boot-deps-cache*
    ) ;define

    ;; 递归解析测试文件、引导文件、resources/ 及 @test-depends 全部依赖，按字母序返回
    (define (find-test-dependencies test-file)
      (if (not (file-exists? test-file))
        '()
        (let ((visited (make-hash-table)))
          (for-each (lambda (p) (%add-dep! visited p)) (%get-boot-deps))
          (%visit-scm-into! visited test-file)
          (let ((res-dir (%find-resources-dir test-file)))
            (when res-dir
              (for-each (lambda (p) (%add-dep! visited p)) (%cached-dir-files res-dir))
            ) ;when
          ) ;let
          (for-each
            (lambda (spec)
              (cond
               ((path-dir? (path spec))
                (for-each (lambda (p) (%add-dep! visited p)) (%collect-dir-files spec))
               ) ;
               ((file-exists? spec) (%add-dep! visited (%norm-path spec)))
              ) ;cond
            ) ;lambda
            (%extract-test-depends test-file)
          ) ;for-each
          (list-sort string<? (hash-table-keys visited))
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
