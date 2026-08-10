;;; exp-library.scm
;;; Expansion-time library: a mutable binding table.
;;;
;;; A library holds top-level bindings (primitives, core forms,
;;; user defines, transformers). Syntax objects carry a reference
;;; to their home library; free identifiers resolve against it.

(define-record-type <exp-library>
  (%make-exp-library name bindings)
  exp-library?
  (name exp-library-name)
  (bindings exp-library-bindings set-exp-library-bindings!))

(define (make-exp-library name)
  (%make-exp-library name '()))

(define (exp-library-ref lib name)
  (let ((entry (assq name (exp-library-bindings lib))))
    (and entry (cdr entry))))

(define (exp-library-define! lib name value)
  (let ((bindings (exp-library-bindings lib)))
    (let ((entry (assq name bindings)))
      (if entry
          (set-cdr! entry value)
          (set-exp-library-bindings! lib (cons (cons name value) bindings))))))

(module-define! the-expander-library 'make-exp-library make-exp-library)
(module-define! the-expander-library 'exp-library? exp-library?)
(module-define! the-expander-library 'exp-library-name exp-library-name)
(module-define! the-expander-library 'exp-library-ref exp-library-ref)
(module-define! the-expander-library 'exp-library-define! exp-library-define!)

(define *base-library* #f)

(define (set-base-library! lib)
  (set! *base-library* lib))

(define (base-library)
  *base-library*)

(module-define! the-expander-library 'set-base-library! set-base-library!)
(module-define! the-expander-library 'base-library base-library)
