(set! (*s7* 'scheme-version) 'r7rs)

(define (file-exists? path)
  (unless (string? path)
    (error 'type-error "(file-exists? path): path should be string"))
  (and (g_access path 0) ; exist?
       (or (g_access path 1) ; have permission?
           (error 'permission-error (string-append "No permission: " path)))))

(define (getenv name)
  (let ((e (assoc name (g_getenvs))))
    (and e (cdr e))))

(define (delete-file path)
  (unless (string? path)
    (error 'type-error "(delete-file path): path should be string"))
  (unless (file-exists? path)
    (error 'read-error (string-append path " does not exist")))
  (g_delete-file path))

;;;

(define (fold fn accum list)
  (if (null? list)
    accum
    (fold fn
          (fn accum (car list))
          (cdr list))))

(define (filter pred l)
  (let loop ([l l])
    (if (null? l)
      l
      (let ([head (car l)]
            [tail (cdr l)])
        (if (pred head)
          (let ([new-tail (loop tail)])
            (if (eq? tail new-tail)
              l
              (cons head new-tail)))
          (loop tail))))))

(define (any pred ls)
  (let lp ([ls ls])
    (cond
      ((null? ls)
       #f)
      ((null? (cdr ls))
       (pred (car ls)))
      (else
        (or (pred (car ls)) (lp (cdr ls)))))))

(define (every pred ls)
  (or (null? ls)
      (and (pred (car ls)) (every pred (cdr ls)))))

(define (sort comparator lst)
  (sort! (copy lst) comparator))

;;;

(define-macro (define-library libname . body)
  `(define ,(symbol (object->string libname))
     (with-let (sublet (unlet)
                 (cons 'import import)
                 (cons '*export* ())
                 (cons 'export
                   (define-macro (,(next-fresh "export-macro") . names)
                     (list 'set!
                           '*export*
                           (list 'append
                                 (list 'quote names)
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

;;; ---------------------------------------------------------------------------
;;; R6RS derived forms the host (s7) does not provide.  The expander kernel
;;; sources (goldfish/expander/kernel) use these freely; at bootstrap-0 they
;;; are evaluated directly by s7, so the seed provides host-side fallbacks.
;;; Once the expander is self-hosted these become OUR macros (lib layer), and
;;; the host forms below are only exercised during the bootstrap.

;;; let*-values : like let-values but binding clauses are evaluated
;;; sequentially (each clause may refer to earlier bindings).  Expand to
;;; nested let-values.
(define-macro (let*-values clauses . body)
  (if (null? clauses)
    `(let () ,@body)
    `(let-values (,(car clauses))
       (let*-values ,(cdr clauses) ,@body))))

;;; ---------------------------------------------------------------------------
;;; Independent record implementation (cf. Guile ice-9/boot-9.scm {Records}).
;;;
;;; The host s7 define-record-type builds inlet-based records: a record is a
;;; let whose first pair is its type tag, so let->list / list traversal of a
;;; record leaks that tag as data (e.g. ((<type> . <type>) (field . ...))).
;;; The expander kernel needs records that cannot be mistaken for association
;;; lists, so records are implemented here as a type descriptor plus a
;;; fixed-layout vector, with type identity tested by eq? on the descriptor
;;; (cf. Guile struct/vtable).
;;;
;;; Layout:  descriptor = (vector 'record-type name fields)
;;;          instance   = (vector descriptor field0 field1 ...)

(define (make-record-type type-name fields)
  (vector 'record-type type-name fields))

(define (record-type? obj)
  (and (vector? obj)
       (positive? (vector-length obj))
       (eq? (vector-ref obj 0) 'record-type)))

(define (record-type-name rtd)
  (vector-ref rtd 1))

(define (record-type-fields rtd)
  (vector-ref rtd 2))

;;; record-instance? : any -> bool
;;; True for vector-layout record instances (first element is a type
;;; descriptor).  Code that must tell records apart from ordinary vectors
;;; (e.g. the expander's stx-vector?, which recurses into container
;;; vectors) uses this to exclude records.

(define (record-instance? obj)
  (and (vector? obj)
       (positive? (vector-length obj))
       (record-type? (vector-ref obj 0))))

(define (record-predicate rtd)
  (lambda (obj)
    (and (vector? obj)
         (positive? (vector-length obj))
         (eq? (vector-ref obj 0) rtd))))

(define (record-field-index rtd field)
  (let loop ((fs (record-type-fields rtd)) (i 1))
    (cond ((null? fs) (error "record: no such field" field))
          ((eq? (car fs) field) i)
          (else (loop (cdr fs) (+ i 1))))))

(define (record-accessor rtd field)
  (let ((idx (record-field-index rtd field)))
    (lambda (obj) (vector-ref obj idx))))

(define (record-modifier rtd field)
  (let ((idx (record-field-index rtd field)))
    (lambda (obj val) (vector-set! obj idx val))))

;;; define-record-type as a host macro (bootstrap-0): expands to code that
;;; builds a descriptor plus vector-layout constructor / predicate /
;;; accessors / modifiers.  The constructor takes one argument per field,
;;; in declaration order (all kernel and library uses are of this shape).
;;; The rtd name is a fresh READABLE symbol (counter-based, not s7's
;;; {gensym}-N:M): the expander artifact is written as Scheme source and
;;; must round-trip through the R7RS reader.

;;; Fresh READABLE symbols (counter-based, not s7's {gensym}-N:M): the
;;; expander artifact is written as Scheme source and must round-trip
;;; through the R7RS reader, so names generated by host macros and by
;;; make-fresh-name are ordinary symbols.

(define *fresh-counter* 0)
(define (next-fresh stem)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append stem "~" (number->string *fresh-counter*))))

(define (next-record-rtd)
  (next-fresh "rtd"))

(define-macro (define-record-type type make ? . fields)
  (let ((rtd (next-record-rtd))
        (make-name (car make))
        (make-params (cdr make))
        (field-names (map car fields))
        (acc-defs
          (let loop ((fs fields) (i 1))
            (if (null? fs)
              '()
              (let ((acc (cadr (car fs))))
                (cons `(define (,acc obj) (vector-ref obj ,i))
                      (if (pair? (cddr (car fs)))
                        (let ((mod (caddr (car fs))))
                          (cons `(define (,mod obj val) (vector-set! obj ,i val))
                                (loop (cdr fs) (+ i 1))))
                        (loop (cdr fs) (+ i 1)))))))))
    `(begin
       (define ,rtd (make-record-type ',type ',field-names))
       (define (,make-name ,@make-params) (vector ,rtd ,@make-params))
       (define (,? obj)
         (and (vector? obj)
              (positive? (vector-length obj))
              (eq? (vector-ref obj 0) ,rtd)))
       ,@acc-defs)))

;;; ---------------------------------------------------------------------------
;;; Expander runtime substrate (seed prelude).  Host-compatible value surface
;;; + runtime module substrate.  All data that reaches the expander comes from
;;; our own R7RS reader (read-forms), which emits (quote ...) with the plain
;;; quote symbol -- never s7's internal #_quote object -- so no s7 reader
;;; workaround is needed here.

(define (vector-map f v . more)
  (let* ((vs (cons v more))
         (n (vector-length v))
         (result (make-vector n)))
    (let loop ((i 0))
      (unless (= i n)
        (vector-set! result i
                     (apply f (map (lambda (v) (vector-ref v i)) vs)))
        (loop (+ i 1))))
    result))

(define (vector-for-each f v . more)
  (let* ((vs (cons v more))
         (n (vector-length v)))
    (let loop ((i 0))
      (unless (= i n)
        (apply f (map (lambda (v) (vector-ref v i)) vs))
        (loop (+ i 1))))))

(define (make-fresh-name stem)
  (next-fresh (symbol->string stem)))

;;; Promises (r7rs-small).  The host's r7rs mode provides make-promise /
;;; force / promise?, but its make-promise is value-semantics, so the R7RS
;;; delay desugar (make-promise (lambda () expr)) would store the thunk as
;;; the value.  delay / delay-force therefore construct a lazy promise in the
;;; host's representation directly; the host's force evaluates it once and
;;; caches (and recursively forces a promise-valued result).

(define (make-lazy-promise thunk)
  (list (cons #f thunk) '+promise+))

;; The host does not provide force (or promise?) in this configuration, so
;; implement the R7RS semantics for the make-lazy-promise representation
;; above: evaluate once, cache, and recursively force a promise-valued
;; result.

(define (force promise)
  (if (and (pair? promise)
           (pair? (cdr promise))
           (eq? (cadr promise) '+promise+))
    (let ((box (car promise)))
      (if (car box)
        (cdr box)
        (let ((value ((cdr box))))
          (set-car! box #t)
          (if (and (pair? value)
                   (pair? (cdr value))
                   (eq? (cadr value) '+promise+))
            (let ((result (force value)))
              (set-cdr! box result)
              result)
            (begin
              (set-cdr! box value)
              value)))))
    promise))

;;; r7rs-small procedures the host does not provide.

;; The host has no eof-object procedure (only the eof-object? predicate),
;; so construct the EOF object in pure Scheme: reading past the end of an
;; empty string returns the single EOF object.  The binding is captured once
;; here so every call returns the same object.

(define *eof-object* (read (open-input-string "")))

(define (eof-object)
  *eof-object*)

(define (bytevector->u8-list bv)
  (let loop ((i (- (bytevector-length bv) 1)) (acc '()))
    (if (< i 0)
      acc
      (loop (- i 1) (cons (bytevector-u8-ref bv i) acc)))))

(define (u8-list->bytevector lst)
  (let* ((bv (make-bytevector (length lst))))
    (let loop ((i 0) (l lst))
      (if (null? l)
        bv
        (begin (bytevector-u8-set! bv i (car l))
               (loop (+ i 1) (cdr l)))))))

(define (floor/ n d)
  (values (floor-quotient n d) (floor-remainder n d)))

(define (truncate/ n d)
  (values (truncate-quotient n d) (truncate-remainder n d)))

(define (syntax-error msg . irritants)
  (apply error (cons (string-append "syntax error: " msg) irritants)))

;;; Runtime module substrate.
;;;
;;; A module is an s7 inlet holding its bindings plus '__name and
;;; '__exports metadata.  module-define! adds a binding and records it
;;; as exported.  the-expander-library (the expander's own API module)
;;; is module instance zero; user R7RS runtime modules use the same
;;; substrate.  module-ref accepts a module object or a registered module
;;; name (the form emitted by the expander for cross-library references).
;;; Note: s7 eval falls back to rootlet for names absent from an inlet, so
;;; evaluating transformer code in the-expander-library documents the
;;; expander API surface without sandboxing it.

(define *module-registry* '())

(define (make-module name)
  (inlet '__name name '__exports '()))

(define (module? obj)
  (and (let? obj) (assq '__name (let->list obj)) #t))

(define (module-name m)
  (let-ref m '__name))

(define (module-exports m)
  (let-ref m '__exports))

(define (module-define! m name value)
  (if (assq name (let->list m))
    (let-set! m name value)
    (varlet m name value))
  (unless (memq name (let-ref m '__exports))
    (let-set! m '__exports (cons name (let-ref m '__exports))))
  m)

(define (module-ref m name)
  (let ((m (if (module? m) m (lookup-module m))))
    (unless (memq name (let-ref m '__exports))
      (error "module-ref: not exported" name))
    (let-ref m name)))

(define (register-module m)
  (let ((name (module-name m)))
    (set! *module-registry*
      (cons (cons name m)
            (filter (lambda (e) (not (equal? (car e) name)))
                    *module-registry*))))
  m)

(define (lookup-module name)
  (let ((entry (assoc name *module-registry*)))
    (unless entry
      (error "lookup-module: unknown module" name))
    (cdr entry)))

(define the-expander-library
  (make-module '(goldfish expander)))

;;; ---------------------------------------------------------------------------
;;; Loader (seed loader).  Module loader driven by our own R7RS reader
;;; (read-forms).  After this bootstrap file, every source file is read by
;;; the Scheme reader -- never by the interpreter -- and evaluated into the
;;; rootlet.  load-source-file reads a file with the R7RS reader and
;;; evaluates each form into the rootlet.

(define (load-find-module-file path)
  (if (file-exists? path)
    path
    (let loop ((dirs *load-path*))
      (if (null? dirs)
        #f
        (let ((candidate (string-append (car dirs) "/" path)))
          (if (file-exists? candidate)
            candidate
            (loop (cdr dirs))))))))

;;; read-forms : port -> (list datum)
;;; Read all datums from a port (through the Scheme `read'), the loader's
;;; input format.  scsyntax's loader reads whole files this way.
;;; This seed definition runs against the bootstrap reader; reader.scm
;;; rebinds read-forms (and read) to the full R7RS reader once it loads.

(define (read-forms port)
  (let loop ((d (read port)) (acc '()))
    (if (eof-object? d)
      (reverse acc)
      (loop (read port) (cons d acc)))))

(define (load-source-file path)
  (let ((file (load-find-module-file path)))
    (unless file
      (error "load-source-file: file not found" path))
    (let ((port (open-input-file file)))
      (dynamic-wind
        (lambda () #f)
        (lambda () (for-each (lambda (f) (eval f (rootlet)))
                             (read-forms port)))
        (lambda () (close-input-port port))))))
