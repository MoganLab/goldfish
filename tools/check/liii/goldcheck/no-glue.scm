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

(define-library (liii goldcheck no-glue)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
  ) ;import
  (export check-no-glue check-no-glue-duplicates)
  (begin

    (define %reader-quote (car (read (open-input-string "'x"))))

    (define (safe-read port)
      (catch #t (lambda () (read port)) (lambda (err-type info) (eof-object)))
    ) ;define

    (define (form-line form . maybe-default)
      (let ((default (if (null? maybe-default) 1 (car maybe-default))))
        (if (pair? form) (or (pair-line-number form) default) default)
      ) ;let
    ) ;define

    (define (glue-symbol? x)
      (and (symbol? x) (string-starts-with? (symbol->string x) "g_"))
    ) ;define

    (define (tagged? form tag)
      (and (pair? form) (eq? (car form) tag))
    ) ;define

    (define (quotation? form)
      (and (pair? form) (or (eq? (car form) 'quote) (eq? (car form) %reader-quote)))
    ) ;define

    (define (%scan-code form line acc)
      (cond ((glue-symbol? form) (cons (list form line) acc))
            ((symbol? form) acc)
            ((quotation? form) acc)
            ((pair? form)
             (let ((current-line (form-line form line)))
               (%scan-code (cdr form) current-line (%scan-code (car form) current-line acc))
             ) ;let
            ) ;
            (else acc)
      ) ;cond
    ) ;define

    (define (scan-code form line)
      (reverse (%scan-code form line '()))
    ) ;define

    (define (definition-name form)
      (and (pair? form)
        (or (eq? (car form) 'define) (eq? (car form) 'define-syntax))
        (pair? (cdr form))
        (let ((target (cadr form)))
          (cond ((symbol? target) target)
                ((and (pair? target) (symbol? (car target))) (car target))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (unique-refs refs)
      (let loop
        ((rest refs) (acc '()))
        (if (null? rest)
          (reverse acc)
          (loop (cdr rest) (if (assoc (caar rest) acc) acc (cons (car rest) acc)))
        ) ;if
      ) ;let
    ) ;define

    (define (scan-export-clause clause exports exported-glue)
      (let ((line (form-line clause)))
        (let loop
          ((entries (cdr clause)) (exports exports) (exported-glue exported-glue))
          (if (null? entries)
            (list exports exported-glue)
            (let* ((entry (car entries))
                   (name
                     (cond ((symbol? entry) entry)
                           ((and (tagged? entry 'rename) (pair? (cdr entry))) (cadr entry))
                           (else #f)
                     ) ;cond
                   ) ;name
                   (glue-names (cond ((symbol? entry) (list entry))
                                     ((tagged? entry 'rename) (cdr entry))
                                     (else '())
                               ) ;cond
                   ) ;glue-names
                  ) ;
              (loop (cdr entries)
                (if name (cons name exports) exports)
                (append exported-glue
                  (map (lambda (glue) (list glue line)) (filter glue-symbol? glue-names))
                ) ;append
              ) ;loop
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (scan-begin-clause clause bindings refs)
      (let loop
        ((body (cdr clause)) (bindings bindings) (refs refs))
        (if (null? body)
          (list bindings refs)
          (let* ((body-form (car body))
                 (name (definition-name body-form))
                 (new-refs (scan-code body-form (form-line body-form)))
                ) ;
            (loop (cdr body)
              (if name (cons name bindings) bindings)
              (append (reverse new-refs) refs)
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; library-info: (path owner-index name bindings exports refs exported-glue)
    (define (scan-library path owner-index form)
      (let ((name (cadr form)))
        (let loop
          ((clauses (cddr form))
           (bindings '())
           (exports '())
           (refs '())
           (exported-glue '())
          ) ;
          (if (null? clauses)
            (list path
              owner-index
              name
              bindings
              exports
              (unique-refs (reverse refs))
              exported-glue
            ) ;list
            (let ((clause (car clauses)))
              (cond
               ((tagged? clause 'export)
                (let ((result (scan-export-clause clause exports exported-glue)))
                  (loop (cdr clauses) bindings (car result) refs (cadr result))
                ) ;let
               ) ;
               ((tagged? clause 'begin)
                (let ((result (scan-begin-clause clause bindings refs)))
                  (loop (cdr clauses) (car result) exports (cadr result) exported-glue)
                ) ;let
               ) ;
               (else (loop (cdr clauses) bindings exports refs exported-glue))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (info-path info)
      (car info)
    ) ;define
    (define (info-owner info)
      (cadr info)
    ) ;define
    (define (info-bindings info)
      (cadddr info)
    ) ;define
    (define (info-exports info)
      (list-ref info 4)
    ) ;define
    (define (info-refs info)
      (list-ref info 5)
    ) ;define
    (define (info-exported-glue info)
      (list-ref info 6)
    ) ;define

    (define (violation path line glue reason)
      (list 'no-glue path line glue reason)
    ) ;define

    (define (library-violations info)
      (map (lambda (entry)
             (violation (info-path info) (cadr entry) (car entry) 'exported-glue)
           ) ;lambda
        (info-exported-glue info)
      ) ;map
    ) ;define

    (define (scan-top-level-form rel owner form)
      (if (and (tagged? form 'define-library) (pair? (cdr form)))
        (let ((info (scan-library rel owner form)))
          (list (+ owner 1) info (library-violations info))
        ) ;let
        (list owner
          #f
          (map (lambda (ref) (violation rel (cadr ref) (car ref) 'outside-library))
            (scan-code form (form-line form))
          ) ;map
        ) ;list
      ) ;if
    ) ;define

    (define (scan-no-glue-file file-path)
      (let ((rel (relativize-path file-path)) (port (open-input-file file-path)))
        (dynamic-wind (lambda () #f)
          (lambda ()
            (let loop
              ((owner 0) (infos '()) (violations '()))
              (let ((form (safe-read port)))
                (if (eof-object? form)
                  (list (reverse infos) (reverse violations))
                  (let* ((state (scan-top-level-form rel owner form))
                         (info (cadr state))
                         (new-violations (caddr state))
                        ) ;
                    (loop (car state)
                      (if info (cons info infos) infos)
                      (append (reverse new-violations) violations)
                    ) ;loop
                  ) ;let*
                ) ;if
              ) ;let
            ) ;let
          ) ;lambda
          (lambda () (close-input-port port))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (check-no-glue file-path)
      "检查 glue 是否只在 define-library 中使用，且未被直接导出。"
      (cadr (scan-no-glue-file file-path))
    ) ;define

    (define (same-owner? a b)
      (and (string=? (info-path a) (info-path b)) (= (info-owner a) (info-owner b)))
    ) ;define

    (define (library-uses-glue? info glue)
      (and (assoc glue (info-refs info)) #t)
    ) ;define

    (define (duplicate-owner? infos info glue)
      (let loop
        ((rest infos))
        (and (not (null? rest))
          (or
            (and (not (same-owner? info (car rest))) (library-uses-glue? (car rest) glue))
            (loop (cdr rest))
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    (define (check-no-glue-duplicates file-paths)
      "检查同一 glue 是否被多个 define-library 使用；file-paths 为路径列表。"
      (let ((infos
              (apply append
                (map (lambda (file-path) (car (scan-no-glue-file file-path))) file-paths)
              ) ;apply
            ) ;infos
           ) ;
        (apply append
          (map
            (lambda (info)
              (let loop
                ((refs (info-refs info)) (acc '()))
                (if (null? refs)
                  (reverse acc)
                  (let ((glue (caar refs)) (line (cadar refs)))
                    (loop (cdr refs)
                      (if (duplicate-owner? infos info glue)
                        (cons (violation (info-path info) line glue 'multiple-libraries) acc)
                        acc
                      ) ;if
                    ) ;loop
                  ) ;let
                ) ;if
              ) ;let
            ) ;lambda
            infos
          ) ;map
        ) ;apply
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
