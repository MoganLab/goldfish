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

(define-library (liii goldfmt scan)
  (export scan scan-string scan-source-string scan-file stem-mode?
    call-with-stem-mode
  ) ;export
  (import (liii base)
    (liii path)
    (liii syntax)
    (liii list)
    (liii goldfmt record)
    (liii goldfmt scan literal)
    (rename (liii goldfmt tokenize)
      (tokenize source-tokenize)
      (tokens->string source-tokens->string)
    ) ;rename
  ) ;import
  (begin
    (define %stem-mode? #f)
    (define (stem-mode?)
      %stem-mode?
    ) ;define
    (define (call-with-stem-mode thunk)
      (let-temporarily ((%stem-mode? #t)) (thunk))
    ) ;define
    (define (raw-string-form? datum)
      (and (pair? datum)
        (eq? (car datum) '*raw-string*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (string? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define
    (define (char-literal-form? datum)
      (and (pair? datum)
        (eq? (car datum) '*char-literal*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (char? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define
    (define (atom? x)
      (or (symbol? x)
        (number? x)
        (string? x)
        (raw-string-literal? x)
        (char-literal? x)
        (boolean? x)
        (char? x)
        (null? x)
        (vector? x)
        (byte-vector? x)
        (eof-object? x)
        (syntax? x)
        (eq? 'undefined? (type-of x))
        (unspecified? x)
        (procedure? x)
      ) ;or
    ) ;define
    (define (scan datum)
      (scan-datum (normalize-datum datum) 0)
    ) ;define
    (define (scan-datum datum depth)
      (cond
       ((raw-string-form? datum)
        (make-atom
          :depth depth
          :value
          (make-raw-string-literal :source (cadr datum) :value (caddr datum))
        ) ;make-atom
       ) ;
       ((char-literal-form? datum)
        (make-atom
          :depth depth
          :value (make-char-literal :source (cadr datum) :value (caddr datum))
        ) ;make-atom
       ) ;
       ((dotted-tail? datum) (scan-datum (dotted-tail-form datum) depth))
       ((atom? datum) (make-atom :depth depth :value datum))
       (else (scan-list datum depth))
      ) ;cond
    ) ;define
    (define (tag? x)
      (or (symbol? x) (and (syntax? x) #t))
    ) ;define
    (define (tag->string x)
      (if (symbol? x) (symbol->string x) (object->string x #f))
    ) ;define
    (define (procedure-name value)
      (and (procedure? value) (object->string value #f))
    ) ;define
    (define (procedure-name=? value name)
      (let ((proc-name (procedure-name value)))
        (and proc-name (string=? proc-name name))
      ) ;let
    ) ;define
    (define (internal-list-values-form? value)
      (and (pair? value) (procedure-name=? (car value) "#_list-values"))
    ) ;define
    (define (internal-apply-values-form? value)
      (and (pair? value) (procedure-name=? (car value) "#_apply-values"))
    ) ;define
    (define (internal-list-to-vector-form? value)
      (and (pair? value)
        (procedure-name=? (car value) "#_list->vector")
        (pair? (cdr value))
        (null? (cddr value))
      ) ;and
    ) ;define
    (define (unquote-apply-values-form? value)
      (and (pair? value)
        (eq? (car value) 'unquote)
        (pair? (cdr value))
        (null? (cddr value))
        (internal-apply-values-form? (cadr value))
      ) ;and
    ) ;define
    (define (internal-list-star-form? value)
      (and (pair? value)
        (let ((name (procedure-name (car value))))
          (and name
            (or (string=? name "<list*>") (string=? name "#_list") (string=? name "list*"))
          ) ;and
        ) ;let
      ) ;and
    ) ;define
    (define (quote-syntax? value)
      (or (eq? value 'quote)
        (and (syntax? value) (string=? (object->string value #f) "#_quote"))
      ) ;or
    ) ;define
    (define (internal-quote-builder-form? value)
      (and (internal-list-values-form? value)
        (pair? (cdr value))
        (pair? (cddr value))
        (null? (cdddr value))
        (quote-syntax? (cadr value))
      ) ;and
    ) ;define
    (define (make-dotted-list head-items tail)
      (if (null? head-items)
        tail
        (cons (car head-items) (make-dotted-list (cdr head-items) tail))
      ) ;if
    ) ;define
    (define (normalize-quasiquote-item datum)
      (cond ((quote-form? datum) (normalize-datum (cadr datum)))
            ((internal-quote-builder-form? datum)
             (list 'quote (normalize-quasiquote-item (caddr datum)))
            ) ;
            ((internal-list-to-vector-form? datum) (normalize-quasiquote-vector datum))
            ((internal-list-values-form? datum) (normalize-quasiquote-list datum))
            ((internal-list-star-form? datum) (normalize-quasiquote-dotted-list datum))
            ((internal-apply-values-form? datum)
             (list 'unquote-splicing (normalize-datum (cadr datum)))
            ) ;
            ((or (number? datum)
               (string? datum)
               (boolean? datum)
               (char? datum)
               (byte-vector? datum)
             ) ;or
             datum
            ) ;
            (else (list 'unquote (normalize-datum datum)))
      ) ;cond
    ) ;define
    (define (normalize-quasiquote-list datum)
      (map normalize-quasiquote-item (cdr datum))
    ) ;define
    (define (normalize-quasiquote-vector datum)
      (list->vector (normalize-quasiquote-list (cadr datum)))
    ) ;define
    (define (normalize-quasiquote-dotted-list datum)
      (let* ((args (cdr datum))
             (prefix-form (if (pair? args) (car args) '()))
             (tail-form (if (pair? (cdr args)) (cadr args) '()))
             (head-items
               (cond ((internal-list-values-form? prefix-form)
                      (map normalize-quasiquote-item (cdr prefix-form))
                     ) ;
                     ((null? prefix-form) '())
                     (else (list (normalize-quasiquote-item prefix-form)))
               ) ;cond
             ) ;head-items
             (tail
               (let ((item (normalize-quasiquote-item tail-form)))
                 (if (unquote-form? item) (make-dotted-tail item) item)
               ) ;let
             ) ;tail
            ) ;
        (make-dotted-list head-items tail)
      ) ;let*
    ) ;define
    (define (explicit-quasiquote-form? datum)
      (and (pair? datum)
        (eq? (car datum) 'quasiquote)
        (pair? (cdr datum))
        (null? (cddr datum))
      ) ;and
    ) ;define
    (define (split-pair-list datum)
      (let loop
        ((current datum) (elems '()))
        (cond
         ((pair? current) (loop (cdr current) (cons (car current) elems)))
         ((null? current) (values (reverse elems) '()))
         (else (values (reverse elems) current))
        ) ;cond
      ) ;let
    ) ;define
    (define (normalize-explicit-qq-template datum)
      (cond ((unquote-form? datum) (normalize-datum datum))
            ((pair? datum)
             (call-with-values (lambda () (split-pair-list datum))
               (lambda (elems tail)
                 (let ((len (length elems)))
                   (cond
                    ((and (null? tail) (>= len 3) (eq? (list-ref elems (- len 2)) 'unquote))
                     (make-dotted-list (map normalize-explicit-qq-template (take elems (- len 2)))
                       (make-dotted-tail
                         (list 'unquote (normalize-datum (list-ref elems (- len 1))))
                       ) ;make-dotted-tail
                     ) ;make-dotted-list
                    ) ;
                    ((null? tail) (map normalize-explicit-qq-template elems))
                    (else (make-dotted-list (map normalize-explicit-qq-template elems)
                            (normalize-explicit-qq-template tail)
                          ) ;make-dotted-list
                    ) ;else
                   ) ;cond
                 ) ;let
               ) ;lambda
             ) ;call-with-values
            ) ;
            (else datum)
      ) ;cond
    ) ;define
    (define (normalize-datum datum)
      (cond ((raw-string-form? datum)
             (make-raw-string-literal :source (cadr datum) :value (caddr datum))
            ) ;
            ((char-literal-form? datum)
             (make-char-literal :source (cadr datum) :value (caddr datum))
            ) ;
            ((quote-form? datum)
             (if (stem-mode?)
               (list 'quote (normalize-datum (cadr datum)))
               (list (car datum) (normalize-datum (cadr datum)))
             ) ;if
            ) ;
            ((and (not (stem-mode?)) (explicit-quasiquote-form? datum))
             (list 'quasiquote (normalize-explicit-qq-template (cadr datum)))
            ) ;
            ((internal-list-values-form? datum)
             (list 'quasiquote (normalize-quasiquote-list datum))
            ) ;
            ((internal-list-to-vector-form? datum)
             (list 'quasiquote (normalize-quasiquote-vector datum))
            ) ;
            ((internal-list-star-form? datum)
             (list 'quasiquote (normalize-quasiquote-dotted-list datum))
            ) ;
            ((unquote-apply-values-form? datum)
             (list 'unquote-splicing (normalize-datum (cadr (cadr datum))))
            ) ;
            ((internal-apply-values-form? datum)
             (list 'unquote-splicing (normalize-datum (cadr datum)))
            ) ;
            ((pair? datum)
             (cons (normalize-datum (car datum)) (normalize-datum (cdr datum)))
            ) ;
            ((byte-vector? datum)
             (let ((result (make-byte-vector (vector-length datum) 0)))
               (let loop
                 ((i 0))
                 (if (>= i (vector-length datum))
                   result
                   (begin
                     (byte-vector-set! result i (normalize-datum (vector-ref datum i)))
                     (loop (+ i 1))
                   ) ;begin
                 ) ;if
               ) ;let
             ) ;let
            ) ;
            ((vector? datum) (list->vector (map normalize-datum (vector->list datum))))
            (else datum)
      ) ;cond
    ) ;define
    (define (s7-internal-form? lst)
      (and (pair? lst)
        (not (null? lst))
        (syntax? (car lst))
        (let ((name (object->string (car lst) #f)))
          (or (string=? name "#_list-values") (string=? name "#_list"))
        ) ;let
      ) ;and
    ) ;define
    (define (dotted-list-elements lst)
      (let loop
        ((current lst) (result '()))
        (cond
         ((pair? current) (loop (cdr current) (cons (car current) result)))
         ((null? current) (reverse result))
         (else
           (reverse (cons current (cons (string->symbol ".") result)))
         ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (scan-list lst depth)
      (let* ((first (car lst)) (rest (cdr lst)))
        (cond
         ((and (not (stem-mode?)) (quote-form? lst))
          (make-env
            :tag-name (if (syntax? first) "#_quote" (symbol->string first))
            :depth    depth
            :children (vector)
            :value    lst
          ) ;make-env
         ) ;
         ((s7-internal-form? lst)
          (make-env
            :tag-name (object->string first #f)
            :depth    depth
            :children (vector)
            :value    lst
          ) ;make-env
         ) ;
         (else
           (let ((children-list (if (dotted-list? lst) (dotted-list-elements lst) (if (tag? first) rest lst))
                 ) ;children-list
                 (has-tag? (and (not (dotted-list? lst)) (tag? first)))
                ) ;
             (let* ((scanned-children
                      (map (lambda (child) (scan-datum child (+ depth 1))) children-list)
                    ) ;scanned-children
                    (children-vec (list->vector scanned-children))
                   ) ;
               (if has-tag?
                 (make-env
                   :tag-name (tag->string first)
                   :depth    depth
                   :children children-vec
                   :value    lst
                 ) ;make-env
                 (make-env :tag-name "" :depth depth :children children-vec :value lst)
               ) ;if
             ) ;let*
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (scan-string-fast str)
      (let ((port (open-input-string str)))
        (let loop
          ((results '()))
          (let ((datum (read port)))
            (if (eof-object? datum)
              (list->vector (reverse results))
              (loop (cons (scan-datum (normalize-datum datum) 0) results))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (scan-string str)
      (if (needs-rewrite-reader-literals? str)
        (scan-string-fast (rewrite-reader-literals str))
        (scan-string-fast str)
      ) ;if
    ) ;define
    (define (scan-source-string raw-content)
      (let* ((scanned (source-tokenize raw-content))
             (leading-blanks
               (let loop
                 ((i 0) (count 0))
                 (if (>= i (string-length raw-content))
                   count
                   (let ((c (string-ref raw-content i)))
                     (cond ((char=? c #\newline) (loop (+ i 1) (+ count 1)))
                           ((char=? c #\return) (loop (+ i 1) count))
                           ((or (char=? c #\space) (char=? c #\tab)) (loop (+ i 1) count))
                           (else count)
                     ) ;cond
                   ) ;let
                 ) ;if
               ) ;let
             ) ;leading-blanks
             (tokens-with-leading (if (> leading-blanks 0) (cons (cons 'newline leading-blanks) scanned) scanned)
             ) ;tokens-with-leading
             (tokens
               (if
                 (and (not (null? tokens-with-leading))
                   (> (string-length raw-content) 0)
                   (char=? (string-ref raw-content (- (string-length raw-content) 1)) #\newline)
                 ) ;and
                 (append tokens-with-leading (list (cons 'newline 1)))
                 tokens-with-leading
               ) ;if
             ) ;tokens
             (processed-content (source-tokens->string tokens))
            ) ;
        (scan-string processed-content)
      ) ;let*
    ) ;define
    (define (scan-file path)
      (scan-source-string (path-read-text path))
    ) ;define
  ) ;begin
) ;define-library
