;;; exp-library.scm
;;; Expansion-time library: a mutable binding table.
;;;
;;; A library holds top-level bindings (primitives, core forms,
;;; user defines, transformers). Syntax objects carry a reference
;;; to their home library; free identifiers resolve against it.
;;;
;;; The binding table is a vector of buckets (each a small assoc list)
;;; indexed by a cheap symbol hash: libraries grow to ~10^3 bindings
;;; (imports copy the source's export table), and identifier resolution
;;; refs that table constantly, so a flat assoc list makes each ref scan
;;; the whole table.  Uses only the pure-Scheme primitives the seed already
;;; runs on (no hash-table dependency); exp-library-bindings materializes
;;; the alist for the few enumeration callers.

(define-record-type/public <exp-library>
  (%make-exp-library name buckets)
  exp-library?
  (name exp-library-name)
  (buckets exp-library-buckets set-exp-library-buckets!))

(define el-map-bucket-count 256)

(define (make-exp-library name)
  (%make-exp-library name (make-vector el-map-bucket-count '())))

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

(define (exp-library-ref lib name)
  ;; name is a symbol for real lookups, but resolve-identifier can hand this
  ;; a raw syntax object (its context-resolve fall-through) -- assq tolerated
  ;; any key by simply never matching, so a non-symbol reads as #f.
  (if (symbol? name)
    (let ((e (assq name
                   (vector-ref (exp-library-buckets lib)
                               (el-map-hash name)))))
      (and e (cdr e)))
    #f))

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
