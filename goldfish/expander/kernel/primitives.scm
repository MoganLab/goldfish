;;; Commentary
;;;
;;; Base library assembly.  Installs exactly the irreducible kernel:
;;;   1. core-form handlers and primitive bindings
;;; Everything else -- including cond-expand and syntax-case, which Racket
;;; likewise derives in user space (lib/cond-expand.scm, and Racket's
;;; collects/racket/private/stxloc.rkt) -- is ordinary object-level source
;;; expanded by the expander itself (lib/*.scm, see driver.scm).
;;;
;;; Syntax objects carry a reference to their home library; free identifiers
;;; resolve against it when scope-based (lexical) lookup fails.

(define primitive-variables
  ;; The r7rs-small (scheme base) procedure set, plus the names re-exported
  ;; by the other standard libraries ((scheme write) / (scheme char) /
  ;; (scheme complex) / (scheme cxr) / (scheme eval) / (scheme file) /
  ;; (scheme inexact) / (scheme lazy) / (scheme load) / (scheme process-
  ;; context) / (scheme r5rs) / (scheme repl) / (scheme time)) and the
  ;; expand-time syntax API.  All resolve against the host (s7) or the
  ;; runtime substrate (common/prelude.scm) at evaluation time.
  '(+ - * / quotient remainder modulo
     floor/ floor-quotient floor-remainder truncate/ truncate-quotient
     truncate-remainder
     = < > <= >=
     number? integer? rational? real? complex? exact? inexact?
     zero? positive? negative? odd? even? finite? infinite? nan?
     abs floor ceiling truncate round
     exp log sin cos tan asin acos atan sqrt expt exact-integer-sqrt
     exact inexact exact->inexact inexact->exact rationalize
     number->string string->number
     gcd lcm max min numerator denominator
     not boolean? boolean=?
     cons car cdr set-car! set-cdr!
     caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
     cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     pair? null? list list? make-list length append reverse
     list-tail list-ref list-set! list-copy
     map for-each member memq memv assoc assq assv
     symbol? symbol->string string->symbol symbol=?
     char? char->integer integer->char
     char=? char<? char>? char<=? char>=?
     char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
     char-alphabetic? char-numeric? char-whitespace?
     char-upper-case? char-lower-case? char-upcase char-downcase char-foldcase
     digit-value
     string? make-string string string-length string-ref string-set!
     string=? string<? string>? string<=? string>=?
     string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
     substring string-append string->list list->string string-copy string-copy!
     string-fill! string-upcase string-downcase string-foldcase
     string-map string-for-each string->vector vector->string
     vector? make-vector vector vector-length vector-ref vector-set!
     vector->list list->vector vector-fill! vector-copy vector-copy!
     vector-append vector-map vector-for-each
     ;; (scheme complex)
     angle imag-part magnitude make-polar make-rectangular real-part
     bytevector? make-bytevector bytevector bytevector-length
     bytevector-u8-ref bytevector-u8-set!
     bytevector-copy bytevector-copy! bytevector-append
     bytevector->u8-list u8-list->bytevector utf8->string string->utf8
     procedure? apply values call-with-values
     call/cc call-with-current-continuation
     dynamic-wind force make-promise promise?
     eq? eqv? equal?
     display write write-shared write-simple write-char write-string
     read read-char read-line read-string
     read-u8 read-bytevector read-bytevector! peek-char peek-u8 char-ready?
     write-u8 write-bytevector
     newline eof-object eof-object?
     port? input-port? output-port? textual-port? binary-port?
     input-port-open? output-port-open?
     current-input-port current-output-port current-error-port
     close-port close-input-port close-output-port flush-output-port
     open-input-string open-output-string get-output-string
     open-input-bytevector open-output-bytevector get-output-bytevector
     open-input-file open-output-file
     call-with-input-file call-with-output-file
     with-input-from-file with-output-to-file
     call-with-port
     file-exists? delete-file
     error syntax-error
     ;; (scheme process-context)
     command-line exit emergency-exit get-environment-variable
     get-environment-variables
     ;; (scheme eval)
     eval environment
     ;; (scheme time)
     current-second current-jiffy jiffies-per-second
     ;; (scheme repl)
     interaction-environment
     ;; expand-time syntax API (cf. phases-model stx primitives:
     ;; MKS/LIST/CAR/CDR/SE), used by procedural transformers
     syntax? syntax-e syntax-form syntax-context syntax-library make-syntax
     syntax->datum datum->syntax identifier?
     stx-ctx-empty stx-set-library stx-ctx-at
     free-identifier=? bound-identifier=? generate-temporaries
     make-syntax-introducer syntax-local-introduce syntax-local-value
     local-expand local-binder
     new-defs def-bind! expand-box expand-unbox expand-set-box!))

;;; (goldfish expander) is the implementation kernel module (cf. Guile's
;;; (guile)): core forms + primitives + the implementation's own macros.
;;; The r7rs-small (scheme base) is a separate on-disk library that imports
;;; it and re-exports the standard surface, so user code never touches
;;; (goldfish expander) directly.
(define the-base-library (make-exp-library '(goldfish expander)))

(define (install-core-forms! lib)
  (for-each (lambda (entry)
              (exp-library-define! lib (car entry) (make-core-form-binding (cdr entry))))
            core-form-handlers))

(define (install-primitives! lib)
  (for-each (lambda (name)
              (exp-library-define! lib name (make-primitive-binding name)))
            primitive-variables))

(install-core-forms! the-base-library)
(install-primitives! the-base-library)
(set-base-library! the-base-library)

(define (initial-context)
  (context-empty))

(module-define! the-expander-library 'initial-context initial-context)
(module-define! the-expander-library 'primitive-variables primitive-variables)
(module-define! the-expander-library 'the-base-library the-base-library)
