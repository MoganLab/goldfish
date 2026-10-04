;;; substrate.scm -- expander runtime substrate.
;;;
;;; Runtime support installed before the expander kernel. The lowered kernel
;;; defines its own records, promises, module substrate and fresh-name logic.

;;; ---------------------------------------------------------------------------
;;; Records use a type descriptor and a fixed-layout vector.

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
    (cond ((null? fs) (error 'record "no such field" field))
          ((eq? (car fs) field) i)
          (else (loop (cdr fs) (+ i 1))))))

(define (record-accessor rtd field)
  (let ((idx (record-field-index rtd field)))
    (lambda (obj) (vector-ref obj idx))))

(define (record-modifier rtd field)
  (let ((idx (record-field-index rtd field)))
    (lambda (obj val) (vector-set! obj idx val))))

;;; ---------------------------------------------------------------------------
;;; Counter-based readable symbols let the generated kernel round-trip
;;; through the R7RS reader.
;;;
;;; Two streams, deliberately separate (see store.scm for the other):
;;; this global *fresh-counter* serves call sites with no expansion
;;; context -- generate-temporaries for user macros (match.scm etc.)
;;; and record-type constructors.  Expansion-internal temps with a
;;; context in hand (e.g. intdef's define-values collector) allocate
;;; from the store instead (context-alloc-name), keeping one name per
;;; expansion deterministic.  Do NOT unify them: the API shape (no ctx)
;;; and the determinism requirement pull opposite ways.

(define *fresh-counter* 0)

(define (next-fresh stem)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append stem "~" (number->string *fresh-counter*))))

;;; define-public : define in this file's scope AND register into
;;; the-expander-library, ending the per-symbol module-define! boilerplate
;;; (149 handwritten registrations at its introduction).  Two forms:
;;;   (define-public (f x) body ...)   procedure
;;;   (define-public name expr)        plain value
;;; Requires: expansion-time syntax-rules (bootstrapped expander), and at
;;; eval time that `the-expander-library' / `module-define!' are bound --
;;; true for every kernel file after exp-library.scm.
(define-syntax define-public
  (syntax-rules ()
    ((_ (name . formals) body ...)
     (begin
       (define (name . formals) body ...)
       (module-define! the-expander-library 'name name)))
    ((_ name expr)
     (begin
       (define name expr)
       (module-define! the-expander-library 'name name)))))

(define (next-record-rtd)
  (next-fresh "rtd"))

(define (vector-map f v . more)
  (unless (procedure? f)
    (error 'wrong-type-arg "vector-map: first argument must be a procedure" f))
  ;; Allocate the result after callbacks so re-entry cannot mutate a prior return.
  (list->vector (apply map f (map vector->list (cons v more)))))

(define (vector-for-each f v . more)
  (unless (procedure? f)
    (error 'wrong-type-arg "vector-for-each: first argument must be a procedure" f))
  (apply for-each f (map vector->list (cons v more))))

(define (make-fresh-name stem)
  (next-fresh (symbol->string stem)))

;;; ---------------------------------------------------------------------------
;;; Ordinary delay preserves its value, including a promise. Tail promises
;;; forward their shared state before the next force, without pending memos.

(define (make-lazy-promise thunk . tail?)
  (list (cons #f
              (if (and (pair? tail?) (car tail?))
                thunk
                (lambda () (list (cons #t (thunk)) '+promise+))))
        '+promise+))

(define (force promise)
  (if (and (pair? promise)
           (pair? (cdr promise))
           (eq? (cadr promise) '+promise+))
    (let ((box (car promise)))
      (if (car box)
        (cdr box)
        (let* ((next ((cdr box)))
               ;; A nested force or continuation may have completed this promise.
               (current (car promise)))
          (unless (car current)
            (if (and (pair? next) (pair? (cdr next))
                     (eq? (cadr next) '+promise+))
              (let ((next-state (car next)))
                (set-car! current (car next-state))
                (set-cdr! current (cdr next-state))
                (set-car! next current))
              (begin (set-car! current #t) (set-cdr! current next))))
          (force promise))))
    promise))

;;; ---------------------------------------------------------------------------
;;; r7rs-small procedures the host does not provide.

;; The host has no eof-object procedure (only the eof-object? predicate),
;; so construct the EOF object in pure Scheme: reading past the end of an
;; empty string returns the single EOF object.  The binding is captured once
;; here so every call returns the same object.

(define *eof-object* (read (open-input-string "")))

(define (eof-object)
  *eof-object*)

(define (syntax-error msg . irritants)
  (apply error (cons (string-append "syntax error: " msg) irritants)))

;;; ---------------------------------------------------------------------------
;;; Runtime module substrate.
;;;
;;; A module is a small Scheme-owned vector record.  Its public bindings and
;;; metadata are kept here; native evaluation uses the separate formal eval
;;; environment stored in the record.  The evaluator environment is native.
;;; module-define! adds a binding and records it as exported.
;;; the-expander-library (the expander's own API module) is module instance
;;; zero; user R7RS runtime modules use the same substrate.  module-ref
;;; accepts a module object or a registered module name (the form emitted by
;;; the expander for cross-library references).

(define *module-registry* '())

(define *module-tag* 'goldfish-module)

(define (module-slot m i)
  (vector-ref m i))

(define (module-set-slot! m i value)
  (vector-set! m i value))

(define (make-module name)
  (vector *module-tag* name '() (make-eval-environment) '()))

(define (module? obj)
  (and (vector? obj)
       (= (vector-length obj) 5)
       (eq? (module-slot obj 0) *module-tag*)))

(define (module-name m)
  (module-slot m 1))

(define (module-eval-environment m)
  (module-slot m 3))

(define (module-exports m)
  (module-slot m 2))

(define (module-binding m name)
  (assq name (module-slot m 4)))

(define (module-define! m name value)
  (let ((binding (module-binding m name)))
    (if binding
      (set-cdr! binding value)
      (module-set-slot! m 4
                        (cons (cons name value) (module-slot m 4)))))
  (eval-environment-define! (module-eval-environment m) name value)
  (unless (memq name (module-slot m 2))
    (module-set-slot! m 2 (cons name (module-slot m 2))))
  m)

(define (module-ref m name)
  (let ((m (if (module? m) m (lookup-module m))))
    (unless (memq name (module-slot m 2))
      (error 'module-ref "not exported" name))
    (eval-environment-ref (module-eval-environment m) name)))

(define (module-set m name value)
  (let ((m (if (module? m) m (lookup-module m))))
    (eval-environment-set! (module-eval-environment m) name value)))

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
      (error 'lookup-module "unknown module" name))
    (cdr entry)))

(define the-expander-library
  (make-module '(goldfish)))
