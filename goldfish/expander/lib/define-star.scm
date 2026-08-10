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
                   ;; (lambda (a ... . args)
                   ;;   (let* ((o1 (if (pair? args) (car args) d1))
                   ;;          (args (if (pair? args) (cdr args) '())))
                   ;;     body ...))
                   (let* ((bindings
                           (apply append
                                  (map (lambda (opt)
                                         (let ((name (car (syntax-form opt)))
                                               (default (cadr (syntax-form opt))))
                                           (list (list name
                                                       (list 'if (list 'pair? 'args)
                                                             (list 'car 'args)
                                                             default))
                                                 (list 'args (list 'if (list 'pair? 'args)
                                                                   (list 'cdr 'args)
                                                                   (quote ()))))))
                                       opts)))
                          (formals (append (reverse req) 'args)))
                     (datum->syntax stx
                       (list 'lambda
                             formals
                             (cons 'let* (cons bindings body-syns))))))
               (let ((p (list-ref params-list i)))
                 (if (pair? (syntax->datum p))
                     (loop (+ i 1) req (append opts (list p)))
                     (loop (+ i 1) (append req (list p)) opts))))))))))
