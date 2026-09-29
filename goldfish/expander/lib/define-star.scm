;;; define* and lambda* support optional parameters and keyword arguments.
;;; Optional parameters use (name default); a bare optional name defaults to
;;; #f. Keyword arguments accept both :name and name: forms.
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
         (let loop ((i 0) (opts '()))
           (if (= i n)
               (if (null? opts)
                   ;; (lambda () body ...)
                   (datum->syntax stx
                     (cons 'lambda
                           (cons '() body-syns)))
                   ;; Keyword-capable form (also handles positional args):
                   ;;
                   ;; (lambda args
                   ;;   (define (kw-name sym) ...)
                   ;;   (define (keyword-like? x) ...)
                   ;;   (define (make-keyed-alist args) ...)
                   ;;   (let ((__keyed (make-keyed-alist args)))
                   ;;     (let* ((a (if (assq 'a __keyed)
                   ;;                   (cdr (assq 'a __keyed))
                   ;;                   #f))
                   ;;            (b (if (assq 'b __keyed)
                   ;;                   (cdr (assq 'b __keyed))
                   ;;                   d2))
                   ;;            ...)
                   ;;       body ...)))
                   ;;
                   ;; Missing required and optional arguments bind to #f;
                   ;; any resulting error comes from the body or default.
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
                              (let ((s (symbol->string sym)))
                                (if (char=? (string-ref s 0) #\:)
                                    (string->symbol (substring s 1))
                                    (string->symbol (substring s 0 (- (string-length s) 1)))))))
                          (keyword-like?-datum
                           '(define (keyword-like? x)
                              (and (symbol? x)
                                   (let ((s (symbol->string x)))
                                     (and (> (string-length s) 1)
                                          (or (char=? (string-ref s 0) #\:)
                                              (char=? (string-ref s (- (string-length s) 1)) #\:)))))))
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
                          (lambda-body
                           (append helper-defs
                                   (list (list 'let
                                               (list (list '__keyed
                                                           (list 'make-keyed-alist 'args)))
                                               (cons 'let* (cons bindings body-syns)))))))
                     (datum->syntax stx
                       (cons 'lambda (cons 'args lambda-body)))))
               (let ((p (list-ref params-list i)))
                 (if (pair? (syntax->datum p))
                     (loop (+ i 1) (append opts (list p)))
                     ;; A bare symbol is optional without a default; missing
                     ;; arguments bind to #f.
                     (loop (+ i 1)
                           (append opts
                                   (list (datum->syntax stx
                                          (list (syntax-form p) #f))))))))))))))
