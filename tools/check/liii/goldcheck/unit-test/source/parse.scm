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

(define-library (liii goldcheck unit-test source parse)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii os)
    (liii path)
    (liii string)
    (liii list)
    (liii hash-table)
    (liii comparator)
    (liii gfproject config)
    (liii goldcheck path)
  ) ;import
  (export resolve-repo-file extract-form-definitions
    parse-library-decl-definitions extract-decl-export-entries
    extract-decl-imports parse-library-structure lib-spec->relative-source-path
  ) ;export
  (begin

    (define (find-repo-root)
      (let ((root (gfproject-root)))
        (if (and root (string? root) (not (string=? root "")))
          root
          (let loop
            ((dir (getcwd)))
            (if
              (and (path-dir? (path (path-join dir "goldfish")))
                (path-file? (path (path-join dir "gf_check.json")))
              ) ;and
              dir
              (let ((parent (path->string (path-parent (path dir)))))
                (if (or (string=? parent dir) (string=? parent "/")) #f (loop parent))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (resolve-repo-file rel)
      (if (path-absolute? (path rel))
        rel
        (let ((root (find-repo-root)))
          (if (and root (string? root) (not (string=? root "")))
            (path->string (path-join root rel))
            rel
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (extract-target-name target)
      (cond ((symbol? target) (list (symbol->string target)))
            ((pair? target)
             (if (symbol? (car target)) (list (symbol->string (car target))) '())
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define (extract-record-field-defs field-spec)
      (if (pair? field-spec)
        (let loop
          ((rem (cdr field-spec)) (acc '()))
          (cond ((null? rem) (reverse acc))
                ((symbol? (car rem)) (loop (cdr rem) (cons (symbol->string (car rem)) acc)))
                (else (loop (cdr rem) acc))
          ) ;cond
        ) ;let
        '()
      ) ;if
    ) ;define

    (define (extract-record-type-definitions form)
      (let ((args (cdr form)))
        (if (and (pair? args) (pair? (cdr args)))
          (let* ((ctor-spec (cadr args))
                 (rest1 (cddr args))
                 (ctor-name
                   (cond
                    ((and (pair? ctor-spec) (symbol? (car ctor-spec)))
                     (symbol->string (car ctor-spec))
                    ) ;
                    ((symbol? ctor-spec) (symbol->string ctor-spec))
                    (else #f)
                   ) ;cond
                 ) ;ctor-name
                 (pred-name (and (pair? rest1) (symbol? (car rest1)) (symbol->string (car rest1)))
                 ) ;pred-name
                 (base-names (append (if ctor-name (list ctor-name) '()) (if pred-name (list pred-name) '()))
                 ) ;base-names
                 (field-specs (if (pair? rest1) (cdr rest1) '()))
                ) ;
            (let loop
              ((rem field-specs) (acc base-names))
              (if (null? rem)
                acc
                (loop (cdr rem) (append acc (extract-record-field-defs (car rem))))
              ) ;if
            ) ;let
          ) ;let*
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (extract-form-definitions form)
      (cond ((not (pair? form)) '())
            ((memq (car form) '(define define* define-syntax define-macro))
             (if (pair? (cdr form)) (extract-target-name (cadr form)) '())
            ) ;
            ((memq (car form) '(define-record-type define*-record-type))
             (extract-record-type-definitions form)
            ) ;
            ((eq? (car form) 'begin)
             (let loop
               ((rest (cdr form)) (acc '()))
               (if (null? rest)
                 acc
                 (loop (cdr rest) (append (extract-form-definitions (car rest)) acc))
               ) ;if
             ) ;let
            ) ;
            (else '())
      ) ;cond
    ) ;define

    (define (extract-include-file-defs inc-path)
      (if (and inc-path (path-file? (path inc-path)))
        (catch #t
          (lambda ()
            (call-with-input-file inc-path
              (lambda (p)
                (let rloop
                  ((f (read p)) (iacc '()))
                  (if (eof-object? f)
                    iacc
                    (rloop (read p) (append (extract-form-definitions f) iacc))
                  ) ;if
                ) ;let
              ) ;lambda
            ) ;call-with-input-file
          ) ;lambda
          (lambda (k e) '())
        ) ;catch
        '()
      ) ;if
    ) ;define

    (define (parse-library-decl-definitions decl source-dir)
      (cond ((not (pair? decl)) '())
            ((eq? (car decl) 'begin)
             (let loop
               ((forms (cdr decl)) (acc '()))
               (if (null? forms)
                 acc
                 (loop (cdr forms) (append (extract-form-definitions (car forms)) acc))
               ) ;if
             ) ;let
            ) ;
            ((and (eq? (car decl) 'include) (pair? (cdr decl)))
             (let loop
               ((files (cdr decl)) (acc '()))
               (if (null? files)
                 acc
                 (let* ((inc-file (car files))
                        (inc-path (if (string? inc-file) (path->string (path-join source-dir inc-file)) #f)
                        ) ;inc-path
                       ) ;
                   (loop (cdr files) (append (extract-include-file-defs inc-path) acc))
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            ;; 库体顶层可以直接书写定义形式（等价于隐式 begin 的一部分），
            ;; 例如 (define-library (x) (export a) (import ...) (define a 1))。
            ;; 若不识别这类定义，导出符号会被误判为「重导出」。
            (else (extract-form-definitions decl))
      ) ;cond
    ) ;define

    (define (extract-decl-export-entries decl acc)
      (if (and (pair? decl) (eq? (car decl) 'export))
        (let loop
          ((items (cdr decl)) (cur acc))
          (if (null? items)
            cur
            (let ((item (car items)))
              (cond
               ((symbol? item)
                (let ((s (symbol->string item)))
                  (loop (cdr items) (cons (cons s s) cur))
                ) ;let
               ) ;
               ((and (pair? item) (eq? (car item) 'rename) (= (length item) 3))
                (let ((int (symbol->string (cadr item))) (ext (symbol->string (caddr item))))
                  (loop (cdr items) (cons (cons ext int) cur))
                ) ;let
               ) ;
               (else (loop (cdr items) cur))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
        acc
      ) ;if
    ) ;define

    (define (extract-lib-spec-from-item item)
      (cond ((not (pair? item)) #f)
            ((memq (car item) '(rename only except prefix))
             (if (pair? (cdr item)) (extract-lib-spec-from-item (cadr item)) #f)
            ) ;
            ((let loop
               ((rest item))
               (cond ((null? rest) #t)
                     ((symbol? (car rest)) (loop (cdr rest)))
                     (else #f)
               ) ;cond
             ) ;let
             item
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (extract-decl-imports decl acc)
      (if (and (pair? decl) (eq? (car decl) 'import))
        (let loop
          ((items (cdr decl)) (cur acc))
          (if (null? items)
            cur
            (let ((spec (extract-lib-spec-from-item (car items))))
              (if spec (loop (cdr items) (cons spec cur)) (loop (cdr items) cur))
            ) ;let
          ) ;if
        ) ;let
        acc
      ) ;if
    ) ;define

    (define (process-library-decls decls source-dir)
      (let dloop
        ((rem decls) (exports '()) (imports '()) (defs '()))
        (if (null? rem)
          (list (cons 'exports (reverse exports))
            (cons 'imports (reverse imports))
            (cons 'defs (reverse defs))
          ) ;list
          (let ((decl (car rem)))
            (dloop (cdr rem)
              (extract-decl-export-entries decl exports)
              (extract-decl-imports decl imports)
              (append (parse-library-decl-definitions decl source-dir) defs)
            ) ;dloop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (read-library-structure-from-port port actual-path)
      (let rloop
        ()
        (let ((expr (catch #t (lambda () (read port)) (lambda (k e) (eof-object)))))
          (cond ((eof-object? expr) #f)
                ((and (pair? expr) (eq? (car expr) 'define-library))
                 (let ((source-dir (path->string (path-parent (path actual-path))))
                       (decls (cddr expr))
                      ) ;
                   (process-library-decls decls source-dir)
                 ) ;let
                ) ;
                (else (rloop))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%parse-structure-uncached source-path)
      (let ((actual-path (resolve-repo-file source-path)))
        (if (not (path-file? (path actual-path)))
          #f
          (catch #t
            (lambda ()
              (call-with-input-file actual-path
                (lambda (port) (read-library-structure-from-port port actual-path))
              ) ;call-with-input-file
            ) ;lambda
            (lambda (k e) #f)
          ) ;catch
        ) ;if
      ) ;let
    ) ;define

    ;; 解析结果缓存：同一文件的库结构会被重导出检查反复解析，
    ;; 命中缓存可避免重复读文件。用 assoc-list 而非 hash-table：
    ;; 库结构中不含 #f 元素，assoc+string=? 查重不会歧义，且 assoc-list
    ;; 是纯 pair 结构，对 worker 会话序列化安全。
    (define *struct-cache* '())

    (define (parse-library-structure source-path)
      (let ((hit (assoc source-path *struct-cache* string=?)))
        (if hit
          (cdr hit)
          (let ((struct (%parse-structure-uncached source-path)))
            (set! *struct-cache* (cons (cons source-path struct) *struct-cache*))
            struct
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (lib-spec->relative-source-path spec)
      ;; 库标识符 -> 板块内相对源码路径（如 (scheme base) -> "scheme/base.scm"）。
      ;; 板块根目录由调用方按 r7rs/ goldfish/ library/ 依次尝试。
      (string-append (string-join (map symbol->string spec) "/") ".scm")
    ) ;define

  ) ;begin
) ;define-library
