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
     exp log sin cos tan asin acos atan sqrt expt square exact-integer-sqrt
     exact inexact exact->inexact inexact->exact rationalize
     number->string string->number
     gcd lcm max min numerator denominator
     not boolean? boolean=?
     exact-integer? integer?
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
     bytevector-advance-utf8 utf8-string-length
     procedure? apply values call-with-values
     call/cc call-with-current-continuation
     dynamic-wind force make-promise promise?
     eq? eqv? equal?
     display write write-shared write-simple write-char write-string
     read read-char read-line read-string
     read-u8 read-bytevector read-bytevector! peek-char peek-u8 char-ready?
     write-u8 write-bytevector
     newline eof-object eof-object?
     make-parameter
     catch throw with-exception-handler raise-continuable
     rootlet inlet curlet dynamic-let?
     pi exact-integer-sqrt
     port? input-port? output-port? textual-port? binary-port?
     input-port-open? output-port-open?
     current-input-port current-output-port current-error-port
     close-port close-input-port close-output-port flush-output-port
     open-input-string open-output-string get-output-string
     open-input-bytevector open-output-bytevector get-output-bytevector
     open-input-file open-output-file open-binary-input-file
     open-binary-output-file
     call-with-input-file call-with-output-file
     with-input-from-file with-output-to-file
     call-with-port
     file-exists? delete-file
     error syntax-error raise read-error? file-error?
     ;; (scheme process-context)
     command-line exit emergency-exit get-environment-variable
     get-environment-variables
     ;; (scheme eval)
     eval environment
     ;; (scheme time)
     current-second current-jiffy jiffies-per-second
     ;; (scheme repl)
     interaction-environment
     ;; s7 extension names re-exported by the liii/srfi layers (host surface)
     object->string eval-string signature copy
     keyword? string->keyword symbol->keyword keyword->symbol
     make-hook hook-functions
     with-output-to-string with-input-from-string
     call-with-input-string call-with-output-string
     reverse! format
     any every fold filter proper-list? iota
     ash logand logior lognot logxor integer-length
     getenv
     set set=? set<=? set-fold set-remove set-union
     char-position string-position
     tree-count tree-cyclic? tree-leaves tree-memq tree-set-memq
      hash-table hash-table? hash-table-ref hash-table-size
      make-iterator iterator? iterator-at-end?
      int-vector int-vector? int-vector-ref int-vector-set! make-int-vector
     float-vector float-vector? float-vector-ref float-vector-set!
     make-float-vector
     complex-vector complex-vector? complex-vector-ref complex-vector-set!
     make-complex-vector
     ;; s7 host functions used by internal tests / tools
     s7-ceiling s7-floor s7-round s7-truncate s7-lcm s7-gcd s7-remainder
     s7-modulo s7-sqrt s7-abs s7-expt
     s7-make-hash-table s7-hash-table-ref s7-hash-table-set!
     s7-let-to-list s7-let-ref s7-let-set!
     s7-string-upcase s7-string-downcase
     unspecified unspecified? undefined undefined? record-instance fill! display*
     let? sublet unlet with-let vm-load vm-enter
     random
     ;; C++ glue functions (g_*), exposed in the host rootlet
     g_access g_bytevector-base64-decode g_bytevector-base64-encode g_chdir
     g_getcwd g_getlogin g_getpid g_goldfish-library g_isdir g_isfile
     g_listdir g_load-path g_md5 g_md5-by-file g_mkdir g_os-arch g_os-call g_os-temp-dir
     g_os-type g_path-append-text g_path-copy g_path-getmtime g_path-getsize
     g_path-read-bytes g_path-read-text g_path-touch g_path-write-bytes
     g_path-write-text g_remove-file g_rename g_rmdir g_setenv g_sha1
     g_sha1-by-file g_sha256 g_sha256-by-file g_string-split g_system
     g_unsetenv
     ;; expand-time syntax API (cf. phases-model stx primitives:
     ;; MKS/LIST/CAR/CDR/SE), used by procedural transformers
     syntax? syntax-e syntax-form syntax-context syntax-library make-syntax
     syntax->datum datum->syntax identifier?
     stx-ctx-empty stx-set-library stx-ctx-at
     free-identifier=? bound-identifier=? generate-temporaries
     make-syntax-introducer syntax-local-introduce syntax-local-value
     local-expand local-binder
     new-defs def-bind! expand-box expand-unbox expand-set-box!
     ;; construction-time intro-scope marking (kernel internal)
     stx-ctx-mark-intro current-intro-scope set-current-intro-scope!))

;;; (goldfish) is the implementation library (cf. Guile's (guile)): core
;;; forms + primitives + the implementation's own macros.  The r7rs-small
;;; (scheme base) is a separate on-disk library that imports it and
;;; re-exports the standard surface, so user code never touches (goldfish)
;;; directly.
(define the-base-library (make-exp-library '(goldfish)))

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
