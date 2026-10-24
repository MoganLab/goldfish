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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldcheck record-export)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii string)
    (liii goldcheck path)
  ) ;import
  (export check-record-export)
  (begin

    ;; <xxx> 形状的符号是记录类型的句法标识（R7RS/SRFI 9），
    ;; 不对应运行时的 Scheme 绑定，不得导出，也不得占位定义。
    (define (angle-name? x)
      (and (symbol? x)
        (let ((s (symbol->string x)))
          (and (> (string-length s) 2) (string-starts-with? s "<") (string-ends? s ">"))
        ) ;let
      ) ;and
    ) ;define

    (define (safe-read port)
      (catch #t (lambda () (read port)) (lambda (err-type info) (eof-object)))
    ) ;define

    (define (form-line form)
      (if (pair? form) (or (pair-line-number form) 1) 1)
    ) ;define

    (define (%violation file line msg)
      (list 'define-record-type file line msg)
    ) ;define

    (define (%export-violation file line name)
      (%violation file
        line
        (string-append "record type '" (symbol->string name) "' should not be exported")
      ) ;%violation
    ) ;define

    ;; export 条目可为符号或 (rename inner outer)，两种位置的 <xxx> 均违规
    (define (%check-rename-syms file line syms acc)
      (if (null? syms)
        acc
        (%check-rename-syms file
          line
          (cdr syms)
          (if (angle-name? (car syms))
            (cons (%export-violation file line (car syms)) acc)
            acc
          ) ;if
        ) ;%check-rename-syms
      ) ;if
    ) ;define

    (define (%check-export-entries file line entries acc)
      (let loop
        ((rest entries) (acc acc))
        (if (null? rest)
          acc
          (let ((e (car rest)))
            (cond
             ((angle-name? e) (loop (cdr rest) (cons (%export-violation file line e) acc)))
             ((and (pair? e) (eq? (car e) 'rename))
              (loop (cdr rest) (%check-rename-syms file line (cdr e) acc))
             ) ;
             (else (loop (cdr rest) acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 占位定义 (define <xxx> '<xxx>)：自引用 quote，用于规避导出检查。
    ;; s7 reader 把糖 'x 读作 (#_quote x)，其 car 是稳定的 syntactic 符号，
    ;; 不能用 'quote 比对，也不能 symbol->string；这里用新鲜读入的 'x 取样，
    ;; 并同时接受字面 (quote x) 形式。
    (define %reader-quote (car (read (open-input-string "'x"))))

    (define (%quoted-self? name x)
      (and (pair? x)
        (or (eq? (car x) 'quote) (eq? (car x) %reader-quote))
        (pair? (cdr x))
        (null? (cddr x))
        (eq? (cadr x) name)
      ) ;and
    ) ;define

    (define (%placeholder-violation file line name)
      (%violation file
        line
        (string-append "record type '"
          (symbol->string name)
          "' placeholder definition should be removed"
        ) ;string-append
      ) ;%violation
    ) ;define

    ;; 扫描 (begin ...) 直接子表：仅占位定义 (define <xxx> '<xxx>) 违规
    (define (%check-begin file body acc)
      (let loop
        ((rest body) (acc acc))
        (if (null? rest)
          acc
          (let ((form (car rest)))
            (if (and (pair? form)
                  (eq? (car form) 'define)
                  (pair? (cdr form))
                  (angle-name? (cadr form))
                  (pair? (cddr form))
                  (%quoted-self? (cadr form) (caddr form))
                ) ;and
              (loop (cdr rest)
                (cons (%placeholder-violation file (form-line form) (cadr form)) acc)
              ) ;loop
              (loop (cdr rest) acc)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%scan-library file form acc)
      (let loop
        ((rest (cddr form)) (acc acc))
        (if (null? rest)
          acc
          (let ((clause (car rest)))
            (cond
             ((and (pair? clause) (eq? (car clause) 'export))
              (loop (cdr rest)
                (%check-export-entries file (form-line clause) (cdr clause) acc)
              ) ;loop
             ) ;
             ((and (pair? clause) (eq? (car clause) 'begin))
              (loop (cdr rest) (%check-begin file (cdr clause) acc))
             ) ;
             (else (loop (cdr rest) acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%record-export-loop port file acc)
      (let ((form (safe-read port)))
        (if (eof-object? form)
          (reverse acc)
          (%record-export-loop port
            file
            (if (and (pair? form) (eq? (car form) 'define-library) (pair? (cdr form)))
              (%scan-library file form acc)
              acc
            ) ;if
          ) ;%record-export-loop
        ) ;if
      ) ;let
    ) ;define

    (define (check-record-export file-path)
      "检查文件中 define-library 的 <xxx> 记录类型句法标识错误导出与占位定义。\n规则一：R7RS/SRFI 9 中 (define-record-type <t> ...) 的 <t> 仅是句法标识，\n不对应运行时绑定，不得出现在 (export ...) 中（含 rename 两个位置，\n转发导出同样违规）；规则二：(define <xxx> '<xxx>) 占位定义应删除。\n返回违规列表，每项形如 (list 'define-record-type rel-path line msg)。"
      (let ((rel (relativize-path file-path)))
        (let ((port (open-input-file file-path)))
          (dynamic-wind (lambda () #f)
            (lambda () (%record-export-loop port rel '()))
            (lambda () (close-input-port port))
          ) ;dynamic-wind
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
