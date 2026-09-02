;;; exp-library.scm
;;; Expansion-time library: a mutable binding table.
;;;
;;; A library holds top-level bindings (primitives, core forms,
;;; user defines, transformers). Syntax objects carry a reference
;;; to their home library; free identifiers resolve against it.
;;;
;;; The binding table is a vector of buckets (each a small assoc list)
;;; indexed by a cheap symbol hash: a library's own defines grow to
;;; ~10^2 bindings, and identifier resolution refs that table constantly,
;;; so a flat assoc list would make each ref scan the whole table.  Uses
;;; only the pure-Scheme primitives the seed already runs on (no
;;; hash-table dependency); exp-library-bindings materializes the alist
;;; for the few enumeration callers.
;;;
;;; Imports are shared views, not copies: importing a library pushes an
;;; interface library (an exp-library whose OWN buckets are the source's
;;; exported bindings, built once and shared by every importer) onto the
;;; target's `uses'.  A library therefore holds only its own defines;
;;; resolution walks own, then each use in turn, then the base library
;;; (the shared implementation substrate; programs stop before it).  This
;;; mirrors Guile, where a module stores its own bindings and importers
;;; reference the module's single public interface instead of copying.

(define-record-type/public <exp-library>
  (%make-exp-library name buckets uses)
  exp-library?
  (name exp-library-name)
  (buckets exp-library-buckets set-exp-library-buckets!)
  (uses exp-library-uses set-exp-library-uses!))

(define el-map-bucket-count 256)

(define (make-exp-library name)
  (%make-exp-library name (make-vector el-map-bucket-count '()) '()))

;; el-map-hash : symbol -> bucket index
;; djb2-style over the symbol's printed name; cheap for the identifiers a
;; library actually binds (length ~1..30), spread by the 256 buckets.
(define (el-map-hash name)
  (let ((s (symbol->string name)))
    (let ((n (string-length s)))
      (let loop ((i 0) (h 0))
        (if (= i n)
          (modulo h el-map-bucket-count)
          (loop (+ i 1) (+ (* h 33) (char->integer (string-ref s i)))))))))

;; exp-library-ref-own : lib name -> binding/#f
;; Lookup in the library's own table only.
(define (exp-library-ref-own lib name)
  (if (symbol? name)
    (let ((e (assq name
                   (vector-ref (exp-library-buckets lib)
                               (el-map-hash name)))))
      (and e (cdr e)))
    #f))

;; exp-library-use-ref : lib name -> binding/#f
;; Lookup across the library's shared import views, newest first (the
;; most recent import of a name shadows earlier ones, matching the old
;; copy-into-own model where the last import overwrote).
(define (exp-library-use-ref lib name)
  (let loop ((uses (exp-library-uses lib)))
    (if (pair? uses)
      (or (exp-library-ref-own (car uses) name)
          (loop (cdr uses)))
      #f)))

;; exp-library-add-use! : lib view -> void
;; Record an imported interface (a shared export snapshot, itself an
;; exp-library).  Idempotent: re-importing the same view is a no-op.
(define (exp-library-add-use! lib view)
  (if (memq view (exp-library-uses lib))
    #f
    (set-exp-library-uses! lib (cons view (exp-library-uses lib)))))

;; exp-library-ref-strict : lib name -> binding/#f
;; What a program library sees: its own defines plus its imports, never
;; the ambient base (R7RS 5.1: a program's environment is exactly its
;; imports).  Used by resolve-identifier for program libraries.
(define (exp-library-ref-strict lib name)
  (or (exp-library-ref-own lib name)
      (exp-library-use-ref lib name)))

(define (exp-library-ref lib name)
  ;; own defines, then the shared import views, then the base
  ;; (implementation) library: real libraries share the implementation
  ;; substrate instead of each copying its ~830 bindings at import.
  ;; Program strictness is enforced by resolve-identifier, which uses
  ;; exp-library-ref-strict for program libraries.
  (or (exp-library-ref-strict lib name)
      (let ((base *base-library*))
        (and base
             (not (eq? lib base))
             (exp-library-ref-own base name)))))

(define (exp-library-define! lib name value)
  (let* ((buckets (exp-library-buckets lib))
         (h (el-map-hash name)))
    (let ((e (assq name (vector-ref buckets h))))
      (if e
        (set-cdr! e value)
        (vector-set! buckets h
                     (cons (cons name value) (vector-ref buckets h)))))))

;; exp-library-bindings : lib -> alist
;; Materialize the bucket vector into an (name . value) alist.  Used by the
;; few enumeration callers (kernel capture, le-rootlet-copy, build-combined);
;; order is deterministic but not insertion order.
(define (exp-library-bindings lib)
  (let ((buckets (exp-library-buckets lib)))
    (let loop ((i 0) (acc '()))
      (if (= i el-map-bucket-count)
        acc
        (loop (+ i 1) (append (vector-ref buckets i) acc))))))

;; set-exp-library-bindings! : lib (list (name . value)) -> void
;; Rebuild the table from an alist (build-combined filters the base
;; library's bindings this way).
(define (set-exp-library-bindings! lib entries)
  (let ((buckets (make-vector el-map-bucket-count '())))
    (for-each (lambda (e)
                (let ((h (el-map-hash (car e))))
                  (vector-set! buckets h (cons e (vector-ref buckets h)))))
              entries)
    (set-exp-library-buckets! lib buckets)))


(define *base-library* #f)

(define (set-base-library! lib)
  (set! *base-library* lib))

(define (base-library)
  *base-library*)
