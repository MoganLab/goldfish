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

    (define (form-line form . default-line)
      (if (pair? form)
        (or (pair-line-number form) (if (null? default-line) 1 (car default-line)))
        (if (null? default-line) 1 (car default-line))
      ) ;if
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
    (define %reader-quasiquote (car (read (open-input-string "`x"))))

    (define (%quoted-form? form)
      (or (eq? (car form) 'quote)
        (eq? (car form) 'quasiquote)
        (eq? (car form) %reader-quote)
        (eq? (car form) %reader-quasiquote)
      ) ;or
    ) ;define

    (define (%type-name-violation file line name missing?)
      (%violation file
        line
        (if missing?
          "record type name is missing; expected <xxx> shape"
          (string-append "record type name "
            (object->string name)
            " must use <xxx> shape"
          ) ;string-append
        ) ;if
      ) ;%violation
    ) ;define

    ;; 递归扫描真实代码句法树；quoted 数据与 quasiquote 模板不是可执行定义。
    (define (%scan-nested file form acc default-line)
      (if (or (not (pair? form)) (%quoted-form? form))
        acc
        (let ((acc
                (if (eq? (car form) 'define-record-type)
                  (let ((missing? (not (pair? (cdr form))))
                        (name (if (pair? (cdr form)) (cadr form) #f))
                       ) ;
                    (if (or missing? (not (angle-name? name)))
                      (cons (%type-name-violation file (form-line form default-line) name missing?)
                        acc
                      ) ;cons
                      acc
                    ) ;if
                  ) ;let
                  acc
                ) ;if
              ) ;acc
             ) ;
          (let loop
            ((rest form) (acc acc))
            (cond ((null? rest) acc)
                  ((pair? rest) (loop (cdr rest) (%scan-nested file (car rest) acc default-line)))
                  (else (%scan-nested file rest acc default-line))
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define

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

    ;; begin 的直接子表保留占位定义检查，同时递归发现局部记录定义。
    (define (%check-begin file body acc default-line)
      (let loop
        ((rest body) (acc acc))
        (cond ((null? rest) acc)
              ((not (pair? rest)) acc)
              (else
                (let* ((form (car rest))
                       (acc
                         (if (and (pair? form)
                               (eq? (car form) 'define)
                               (pair? (cdr form))
                               (angle-name? (cadr form))
                               (pair? (cddr form))
                               (%quoted-self? (cadr form) (caddr form))
                             ) ;and
                           (cons (%placeholder-violation file (form-line form) (cadr form)) acc)
                           acc
                         ) ;if
                       ) ;acc
                      ) ;
                  (loop (cdr rest) (%scan-nested file form acc default-line))
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-library file form acc default-line)
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
              (loop (cdr rest) (%check-begin file (cdr clause) acc default-line))
             ) ;
             (else (loop (cdr rest) (%scan-nested file clause acc default-line)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%record-export-loop port file acc default-line)
      (let* ((form (safe-read port)) (next-default-line (+ (port-line-number port) 1)))
        (if (eof-object? form)
          (reverse acc)
          (%record-export-loop port
            file
            (if (and (pair? form) (eq? (car form) 'define-library) (pair? (cdr form)))
              (%scan-library file form acc default-line)
              (%scan-nested file form acc default-line)
            ) ;if
            next-default-line
          ) ;%record-export-loop
        ) ;if
      ) ;let*
    ) ;define

    (define (check-record-export file-path)
      "检查 define-record-type 类型名及 <xxx> 句法标识的错误导出与占位定义。\n规则一：记录类型名必须采用 <xxx> 形状，普通脚本与嵌套定义同样检查；\n规则二：<xxx> 不得出现在 (export ...) 中（含 rename 两个位置，\n转发导出同样违规）；规则三：(define <xxx> '<xxx>) 占位定义应删除。\n返回违规列表，每项形如 (list 'define-record-type rel-path line msg)。"
      (let ((rel (relativize-path file-path)))
        (let ((port (open-input-file file-path)))
          (dynamic-wind (lambda () #f)
            (lambda () (%record-export-loop port rel '() 1))
            (lambda () (close-input-port port))
          ) ;dynamic-wind
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
