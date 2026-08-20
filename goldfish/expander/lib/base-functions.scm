;;; base-functions.scm -- library functions implemented in Scheme.
;;;
;;; Loaded into the ROOTLET (like Guile's boot-9.scm into the root module)
;;; right after the module system comes up, overriding the s7 primitives of
;;; the same name.  All code -- the expander kernel itself, every library,
;;; and user programs -- resolves `map' / `for-each' by name into the
;;; rootlet, so this single Scheme definition is what everyone calls.  The
;;; multi-list variants use the `apply' primitive for variadic callback
;;; calls (Guile boot-9 style); the rest-list iteration uses explicit
;;; helper recursion so the definition does not depend on `map' being
;;; bound before it is installed.

(define (map f l1 . rest)
  (if (null? rest)
    (let map1 ((l l1))
      (if (pair? l)
        (cons (f (car l)) (map1 (cdr l)))
        '()))
    (let mapn ((l1 l1) (rest rest))
      (if (pair? l1)
        (cons (apply f (car l1) (map-cars rest))
              (mapn (cdr l1) (map-cdrs rest)))
        '()))))

;; car of every rest list.
(define (map-cars rest)
  (if (null? rest)
    '()
    (cons (caar rest) (map-cars (cdr rest)))))

;; cdr of every rest list.
(define (map-cdrs rest)
  (if (null? rest)
    '()
    (cons (cdar rest) (map-cdrs (cdr rest)))))

(define (for-each f l1 . rest)
  (if (null? rest)
    (let fe1 ((l l1))
      (if (not (null? l))
        (begin
          (f (car l))
          (fe1 (cdr l)))))
    (let fen ((l1 l1) (rest rest))
      (if (not (null? l1))
        (begin
          (apply f (car l1) (map-cars rest))
          (fen (cdr l1) (map-cdrs rest)))))))
