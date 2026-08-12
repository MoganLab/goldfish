;;; define-star.scm
;;; s7 define* / lambda* compatibility layer.
;;;
;;;   (define* (f a (b 2) (c 3)) body ...)
;;;
;;; rewrites optional positional parameters to a standard procedure with a
;;; threaded rest list, via lambda*:
;;;
;;;   (lambda* (a (b 2) (c 3)) body ...)
;;;     => (lambda (a . args)
;;;          (let* ((b   (if (pair? args) (car args) 2))
;;;                 (args (if (pair? args) (cdr args) '()))
;;;                 (c   (if (pair? args) (car args) 3)))
;;;            body ...))
;;;
;;; Optional parameters may also be supplied by keyword (:name value), in
;;; any order, mixed with positional args:
;;;
;;;   (f :c 5)         ; b gets its default, c gets 5
;;;   (f 1 :c 5)       ; a=1, b=2, c=5
;;;
;;; The keyword path expands to a self-contained form:
;;;
;;;   (lambda (req ... . args)
;;;     (define (kw-name sym) ...)
;;;     (define (keyword-like? x) ...)
;;;     (define (make-keyed-alist args) ...)
;;;     (let ((__keyed (make-keyed-alist args)))
;;;       (let* ((b (if (assq 'b __keyed) (cdr (assq 'b __keyed)) 2))
;;;              (c (if (assq 'c __keyed) (cdr (assq 'c __keyed)) 3)))
;;;         body ...)))
;;;
;;; make-keyed-alist walks args left to right.  A ':name symbol consumes the
;;; following item as its value (stored under (name . value)).  Any other
;;; item is a positional arg; it is stored under the name of the next
;;; optional parameter in declaration order (a fixed list spliced into the
;;; helper), so each optional parameter's binding needs only a single
;;; inline lookup -- no nested if, and no helper function returns the value.
;;;
;;; The single-level inline lookup avoids an expander problem: core-if
;;; rejects a nested if produced by a macro expansion inside a library body
;;; ("if: expected (if cond then [else])").  Emitting one flat lookup per
;;; optional parameter sidesteps that.  (s7's separate stale opt1_lambda
;;; cache bug -- a lambda returned by a helper and called in a later
;;; invocation binding its captures wrongly -- is fixed in src/s7.c, the
;;; OP_F_NP symbol_ctr special case.)
;;;
;;; Required parameters stay as ordinary formals (so they bind normally in
;;; the body); only the optional parameters are resolved by the inline
;;; lookup.  The helpers (kw-name / keyword-like? / make-keyed-alist) are
;;; emitted inline so the expansion does not depend on any expander-only
;;; helper (which user libraries cannot see).
;;;
;;; define* is a thin syntax-case macro over lambda*.  lambda* must NOT
;;; flatten its body through syntax->datum: in a nested macro expansion
;;; (define* -> lambda*) the macro-use stx carries only the outer macro's
;;; intro scope, so a re-datum'd body would lose its use-site scope and its
;;; free identifiers would fail to resolve.  The body forms and the
;;; required/optional parameter identifiers are therefore spliced into the
;;; output as their original use-site syntax objects (datum->syntax keeps
;;; them untouched); only the macro skeleton (lambda / let* / args /
;;; pair? / car / cdr / if) is built from datum.
;;; Installed by install.scm after syntax-case.

(define-syntax define*
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name . params) body ...)
       #'(define name (lambda* params body ...)))
      ((_ (name . params))
       #'(define (name . params)))
      ((_ name expr)
       #'(define name expr)))))

(define-syntax lambda*
  (lambda (stx)
    (syntax-case stx ()
      ((_ params body ...)
       (let* ((params-list (syntax-form #'params))
              (body-syns (syntax-form #'(body ...)))
              (n (length params-list)))
         (let loop ((i 0) (req '()) (opts '()))
           (if (= i n)
               (if (null? opts)
                   ;; (lambda (a b ...) body ...)
                   (datum->syntax stx
                     (cons 'lambda
                           (cons (reverse req) body-syns)))
                   ;; Keyword-capable form (also handles positional args):
                   ;;
                   ;; (lambda (req ... . args)
                   ;;   (define (kw-name sym) ...)
                   ;;   (define (keyword-like? x) ...)
                   ;;   (define (make-keyed-alist args) ...)
                   ;;   (let ((__keyed (make-keyed-alist args)))
                   ;;     (let* ((o1 (if (assq 'o1 __keyed)
                   ;;                   (cdr (assq 'o1 __keyed))
                   ;;                   d1))
                   ;;            ...)
                   ;;       body ...)))
                   (let* ((opt-names
                           (map (lambda (o) (car (syntax-form o))) opts))
                          (opt-defaults
                           (map (lambda (o) (cadr (syntax-form o))) opts))
                          (bindings
                           (map (lambda (name default)
                                  (list name
                                        (list 'if
                                              (list 'assq (list 'quote name) '__keyed)
                                              (list 'cdr (list 'assq (list 'quote name) '__keyed))
                                              default)))
                                opt-names opt-defaults))
                          (kw-name-datum
                           '(define (kw-name sym)
                              (string->symbol (substring (symbol->string sym) 1))))
                          (keyword-like?-datum
                           '(define (keyword-like? x)
                              (and (symbol? x)
                                   (let ((s (symbol->string x)))
                                     (and (> (string-length s) 1)
                                          (char=? (string-ref s 0) #\:))))))
                          (make-keyed-alist-datum
                           (list 'define 'make-keyed-alist
                                 (list 'lambda '(args)
                                       (list 'let 'loop
                                             (list (list 'rest 'args)
                                                   (list 'pos 0)
                                                   (list 'acc (quote ())))
                                             (list 'cond
                                                   (list (list 'null? 'rest)
                                                         (list 'reverse 'acc))
                                                   (list (list 'keyword-like? (list 'car 'rest))
                                                         (list 'if (list 'null? (list 'cdr 'rest))
                                                               (list 'error "keyword without value" (list 'car 'rest))
                                                               (list 'loop (list 'cddr 'rest)
                                                                     'pos
                                                                     (list 'cons
                                                                           (list 'cons (list 'kw-name (list 'car 'rest))
                                                                                 (list 'cadr 'rest))
                                                                           'acc))))
                                                   (list 'else
                                                         (list 'loop (list 'cdr 'rest)
                                                               (list '+ 'pos 1)
                                                               (list 'cons
                                                                     (list 'cons
                                                                           (list 'list-ref
                                                                                 (list 'quote opt-names)
                                                                                 'pos)
                                                                           (list 'car 'rest))
                                                                     'acc))))))))
                          (helper-defs
                           (list kw-name-datum
                                 keyword-like?-datum
                                 make-keyed-alist-datum))
                          (formals (append (reverse req) 'args))
                          (lambda-body
                           (append helper-defs
                                   (list (list 'let
                                               (list (list '__keyed
                                                           (list 'make-keyed-alist 'args)))
                                               (cons 'let* (cons bindings body-syns)))))))
                     (datum->syntax stx
                       (cons 'lambda (cons formals lambda-body)))))
                 (let ((p (list-ref params-list i)))
                   (if (pair? (syntax->datum p))
                       (loop (+ i 1) req (append opts (list p)))
                       (loop (+ i 1) (cons p req) opts))))))))))
