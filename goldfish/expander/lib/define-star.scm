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
;;; define* is a thin macro over lambda*.  Output is built with with-syntax
;;; + syntax-case templates so the generated nodes carry the macro-template
;;; context (correct scope handling in the library-body scanner).
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
       (let ((params-datum (syntax->datum #'params)))
         (let loop ((ps params-datum) (req '()) (opts '()))
           (if (null? ps)
             (if (null? opts)
               #'(lambda params body ...)
               (let ((bindings
                      (apply append
                             (map (lambda (opt)
                                    (list (list (car opt)
                                                (list 'if (list 'pair? 'args)
                                                      (list 'car 'args)
                                                      (cadr opt)))
                                          (list 'args (list 'if (list 'pair? 'args)
                                                            (list 'cdr 'args)
                                                            (quote ())))))
                                  opts))))
                 (with-syntax ((bindings bindings)
                               (formals (append req (quote args))))
                   #'(lambda formals (let* bindings body ...)))))
             (if (pair? (car ps))
               (loop (cdr ps) req (append opts (list (car ps))))
               (loop (cdr ps) (append req (list (car ps))) opts)))))))))
