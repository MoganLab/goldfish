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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; define-library 文件的只读解析：为 type-hint 规则提供结构数据。
;; 不 eval、不加载被解析的库，只做 read 后的数据遍历。

(define-library (liii goldcheck type-hint parse)
  (import (scheme base) (scheme file) (liii base) (liii list))
  (export read-all-forms find-define-library parse-library-file lib-name
    lib-line lib-imports lib-exports lib-defines def-name def-kind def-arity
    def-line def-expr proper-list? form-line
  ) ;export
  (begin

    (define (safe-read port)
      (catch #t (lambda () (read port)) (lambda (err-type info) (eof-object)))
    ) ;define

    (define (read-all-forms path)
      (let ((port (open-input-file path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ((acc '()))
              (let ((e (safe-read port)))
                (if (eof-object? e) (reverse acc) (loop (cons e acc)))
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-input-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (form-line form)
      (if (pair? form) (or (pair-line-number form) 1) 1)
    ) ;define

    (define (proper-list? x)
      (let loop
        ((l x))
        (cond ((null? l) #t)
              ((pair? l) (loop (cdr l)))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; def 记录: (name kind arity line expr)
    ;; kind : lambda | case-lambda | rest | value
    ;; arity: lambda -> 参数个数; case-lambda -> 各子句参数个数列表; 其他 #f
    (define (def-name d)
      (car d)
    ) ;define
    (define (def-kind d)
      (cadr d)
    ) ;define
    (define (def-arity d)
      (caddr d)
    ) ;define
    (define (def-line d)
      (list-ref d 3)
    ) ;define
    (define (def-expr d)
      (list-ref d 4)
    ) ;define

    ;; 形参表 -> (kind arity)
    (define (formals->kind-arity f)
      (cond ((symbol? f) '(rest #f))
            ((proper-list? f) (list 'lambda (length f)))
            (else '(rest #f))
      ) ;cond
    ) ;define

    (define (case-lambda-kind-arity clauses)
      (let loop
        ((rest clauses) (counts '()))
        (if (null? rest)
          (list 'case-lambda (reverse counts))
          (let ((c (car rest)))
            (if (and (pair? c) (pair? (cdr c)))
              (let ((f (car c)))
                (if (or (symbol? f) (not (proper-list? f)))
                  '(rest #f)
                  (loop (cdr rest) (cons (length f) counts))
                ) ;if
              ) ;let
              '(value #f)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-define d)
      (if (and (pair? (cdr d)) (pair? (cadr d)))
        ;; (define (name formals...) body...)
        (let* ((target (cadr d)) (name (car target)) (ka (formals->kind-arity (cdr target))))
          (if (symbol? name) (list name (car ka) (cadr ka) (form-line d) d) #f)
        ) ;let*
        ;; (define name expr)
        (let* ((name (cadr d))
               (expr
                 (if (and (pair? (cddr d)) (pair? (caddr d))) (caddr d) #f)
               ) ;expr
               (ka
                 (cond ((not (pair? expr)) '(value #f))
                       ((eq? (car expr) 'lambda)
                        (if (pair? (cdr expr)) (formals->kind-arity (cadr expr)) '(value #f))
                       ) ;
                       ((eq? (car expr) 'case-lambda) (case-lambda-kind-arity (cdr expr)))
                       (else '(value #f))
                 ) ;cond
               ) ;ka
              ) ;
          (if (symbol? name) (list name (car ka) (cadr ka) (form-line d) d) #f)
        ) ;let*
      ) ;if
    ) ;define

    ;; begin 内的 define 收集（含嵌套 begin）
    (define (collect-defines forms)
      (let loop
        ((fs forms) (acc '()))
        (if (null? fs)
          (reverse acc)
          (let ((f (car fs)))
            (cond
             ((and (pair? f) (eq? (car f) 'define) (pair? (cdr f)))
              (let ((d (parse-define f)))
                (loop (cdr fs) (if d (cons d acc) acc))
              ) ;let
             ) ;
             ((and (pair? f) (eq? (car f) 'begin))
              (loop (cdr fs) (append acc (collect-defines (cdr f))))
             ) ;
             (else (loop (cdr fs) acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%lib-name-set? s)
      (and (pair? s)
        (let loop
          ((l s))
          (cond ((null? l) #t)
                ((symbol? (car l)) (loop (cdr l)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; import 条目 -> 符号列表；仅识别直接库名形式
    (define (import-set->name s)
      (cond ((symbol? s) (list s))
            ((%lib-name-set? s) s)
            (else #f)
      ) ;cond
    ) ;define

    (define (%import-entries sets)
      (let loop
        ((items (apply append sets)) (acc '()))
        (if (null? items)
          (reverse acc)
          (let ((n (import-set->name (car items))))
            (loop (cdr items) (if n (cons n acc) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; export 条目：符号或 (rename inner outer)，均取对外名字
    (define (%export-entries sets)
      (let loop
        ((items (apply append sets)) (acc '()))
        (if (null? items)
          (reverse acc)
          (let ((e (car items)))
            (loop (cdr items)
              (cond ((symbol? e) (cons e acc))
                    ((and (pair? e)
                       (eq? (car e) 'rename)
                       (pair? (cdr e))
                       (pair? (cddr e))
                       (symbol? (caddr e))
                     ) ;and
                     (cons (caddr e) acc)
                    ) ;
                    (else acc)
              ) ;cond
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-define-library forms)
      (let loop
        ((fs forms))
        (cond ((null? fs) #f)
              ((and (pair? (car fs)) (eq? (caar fs) 'define-library)) (car fs))
              (else (loop (cdr fs)))
        ) ;cond
      ) ;let
    ) ;define

    (define (parse-library-file path)
      "解析 define-library 文件，返回库信息 alist；非库文件返回 #f"
      (let ((lib (find-define-library (read-all-forms path))))
        (if (not lib)
          #f
          (let loop
            ((clauses (cdr lib)) (imports '()) (exports '()) (defines '()))
            (if (null? clauses)
              (list
                (cons 'lib-name
                  (if (and (pair? (cdr lib)) (%lib-name-set? (cadr lib))) (cadr lib) '())
                ) ;cons
                (cons 'line (form-line lib))
                (cons 'imports (%import-entries (reverse imports)))
                (cons 'exports (%export-entries (reverse exports)))
                (cons 'defines defines)
              ) ;list
              (let ((c (car clauses)))
                (cond
                 ((and (pair? c) (eq? (car c) 'import))
                  (loop (cdr clauses) (cons (cdr c) imports) exports defines)
                 ) ;
                 ((and (pair? c) (eq? (car c) 'export))
                  (loop (cdr clauses) imports (cons (cdr c) exports) defines)
                 ) ;
                 ((and (pair? c) (eq? (car c) 'begin))
                  (loop (cdr clauses) imports exports (append defines (collect-defines (cdr c))))
                 ) ;
                 (else (loop (cdr clauses) imports exports defines))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (lib-name info)
      (cdr (assq 'lib-name info))
    ) ;define
    (define (lib-line info)
      (cdr (assq 'line info))
    ) ;define
    (define (lib-imports info)
      (cdr (assq 'imports info))
    ) ;define
    (define (lib-exports info)
      (cdr (assq 'exports info))
    ) ;define
    (define (lib-defines info)
      (cdr (assq 'defines info))
    ) ;define

  ) ;begin
) ;define-library
