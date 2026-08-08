(set! (*s7* 'scheme-version) 'r7rs)

(define (file-exists? path)
  (unless (string? path)
    (error 'type-error "(file-exists? path): path should be string"))
  (and (g_access path 0) ; exist?
       (or (g_access path 1) ; have permission?
           (error 'permission-error (string-append "No permission: " path)))))

(define (delete-file path)
  (unless (string? path)
    (error 'type-error "(delete-file path): path should be string"))
  (unless (file-exists? path)
    (error 'read-error (string-append path " does not exist")))
  (g_delete-file path))

(define-macro (define-library libname . body)
  `(define ,(symbol (object->string libname))
     (with-let (sublet (unlet)
                 (cons 'import import)
                 (cons '*export* ())
                 (cons 'export
                   (define-macro (,(gensym) . names)
                     (#_list-values
                      'set!
                      '*export*
                      (#_list-values
                       'append
                       (#_list-values #_quote names)
                       '*export*)))))
       ,@body
       (apply inlet
         (map (lambda (entry)
                (if (or (member (car entry) '(*export* export import))
                      (and (pair? *export*) (not (member (car entry) *export*))))
                  (values)
                  entry))
           (curlet))))))

(unless (defined? 'r7rs-import-library-filename)
  (define (r7rs-import-library-filename libs)
    (when (pair? libs)
      (let ((lib (if (memq (caar libs) '(only except prefix rename)) (cadar libs) (car libs))))
        (when (not (defined? (symbol (object->string lib))))
          (load (let loop
                  ((parts lib) (name ""))
                  (set! name (string-append name (symbol->string (car parts))))
                  (if (null? (cdr parts))
                    (string-append name ".scm")
                    (begin
                      (set! name (string-append name "/"))
                      (loop (cdr parts) name))))))
        (r7rs-import-library-filename (cdr libs))))))

(define-macro (import . libs)
  `(begin
     (r7rs-import-library-filename (quote ,libs))
     (varlet (curlet)
       ,@(map (lambda (lib)
                (case (car lib)
                      ((only)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (name) (cons name (e name))) names)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      ((except)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (entry)
                                    (if (member (car entry) names)
                                      (values)
                                      entry))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      ((prefix)
                       `((lambda (e prefx)
                           (apply inlet
                             (map (lambda (entry)
                                    (cons
                                      (string->symbol (string-append
                                                        (symbol->string prefx)
                                                        (symbol->string (car entry))))
                                      (cdr entry)))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (caddr (quote ,lib))))
                      ((rename)
                       `((lambda (e names)
                           (apply inlet
                             (map (lambda (entry)
                                    (let ((info (assoc (car entry) names)))
                                      (if info
                                        (cons (cadr info) (cdr entry))
                                        entry)))
                               e)))
                         (symbol->value (symbol (object->string (cadr (quote ,lib)))))
                         (cddr (quote ,lib))))
                      (else `(let ((sym (symbol (object->string (quote ,lib)))))
                               (if (not (defined? sym))
                                 (format () "~A not loaded~%" sym)
                                 (symbol->value sym))))))
           libs))))
