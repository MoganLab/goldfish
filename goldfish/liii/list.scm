(define-library (liii list)
  (import (goldfish))
  (export circular-list
    iota
    xcons
    cons*
    null-list?
    circular-list?
    proper-list?
    dotted-list?
    first
    second
    third
    fourth
    fifth
    sixth
    seventh
    eighth
    ninth
    tenth
    take
    drop
    take-right
    drop-right
    split-at
    last-pair
    last
    zip
    count
    fold
    fold-right
    reduce
    reduce-right
    filter
    partition
    remove
    append-map
    find
    any
    every
    list-index
    take-while
    drop-while
    delete
    alist-cons
    flat-map
    list-null?
    list-not-null?
    not-null-list?
    length=?
    length>?
    length>=?
    flatten
    list-take
    list-drop
    list-take-right
    list-drop-right
  ) ;export
   (import (scheme base)
     (rename (srfi srfi-1) (fold srfi-1-fold))
     (srfi srfi-13) (liii error))
  (begin

    ;; (liii list) re-exports SRFI-1's fold.  The substrate also binds fold
    ;; (a legacy accum-first fold in boot), which would otherwise win the
    ;; name and shadow the SRFI-1 one here; alias it explicitly so the
    ;; exported binding is SRFI-1's.
    (define fold srfi-1-fold)

    (define (length-cmp lst n)
      (let loop ((lst lst) (i 0))
        (cond ((null? lst) i)
              ((pair? lst) (loop (cdr lst) (+ i 1)))
              (else i))))

    (define (length=? x lst)
      (when (not (integer? x)) (type-error "length=?: first parameter x must be an integer"))
      (when (< x 0) (value-error "length=?: expected non-negative integer x but received ~d" x))
      (= x (length-cmp lst x)))

    (define (length>? lst len) (> (length-cmp lst len) len))
    (define (length>=? lst len) (>= (length-cmp lst len) len))

    (define flat-map append-map)

    (define (list-take lst n)
      (unless (list? lst)
        (type-error "list-take: first argument must be a list" lst)
      ) ;unless
      (unless (integer? n)
        (type-error "list-take: second argument must be an integer" n)
      ) ;unless
      (cond ((< n 0) '())
            ((= n 0) '())
            (else (let loop
                    ((rest lst) (count 0) (result '()))
                    (cond ((null? rest) lst)
                          ((>= count n) (reverse result))
                          (else (loop (cdr rest) (+ count 1) (cons (car rest) result)))
                    ) ;cond
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (list-drop lst n)
      (unless (list? lst)
        (type-error "list-drop: first argument must be a list" lst)
      ) ;unless
      (unless (integer? n)
        (type-error "list-drop: second argument must be an integer" n)
      ) ;unless
      (cond ((< n 0) lst)
            ((= n 0) lst)
            (else (let loop
                    ((rest lst) (count 0))
                    (cond ((null? rest) '())
                          ((>= count n) rest)
                          (else (loop (cdr rest) (+ count 1)))
                    ) ;cond
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (list-take-right lst n)
      (unless (list? lst)
        (type-error "list-take-right: first argument must be a list" lst)
      ) ;unless
      (unless (integer? n)
        (type-error "list-take-right: second argument must be an integer" n)
      ) ;unless
      (cond ((< n 0) '())
            ((>= n (length lst)) lst)
            (else (take-right lst n))
      ) ;cond
    ) ;define

    (define (list-drop-right lst n)
      (unless (list? lst)
        (type-error "list-drop-right: first argument must be a list" lst)
      ) ;unless
      (unless (integer? n)
        (type-error "list-drop-right: second argument must be an integer" n)
      ) ;unless
      (cond ((< n 0) lst)
            ((>= n (length lst)) '())
            (else (drop-right lst n))
      ) ;cond
    ) ;define

    (define (not-null-list? l)
      ;; asserts its argument is a list: a non-list raises type-error, the
      ;; empty list and improper lists answer #f.
      (cond ((null? l) #f)
            ((pair? l) (and (list? l) #t))
            (else (type-error "not-null-list?: expected a list, got ~s" l))))
    (define list-null? null?)
    (define (list-not-null? l)
      ;; total predicate: #t iff l is a non-empty proper list.
      (and (not (null? l)) (pair? l) (list? l)))

    ;; One flattening pass: every list element is spliced in place of its own
    ;; brackets; an empty list contributes nothing (its brackets vanish);
    ;; non-list elements are kept.  (flatten lst d) applies this pass d times
    ;; for an integer d >= 0, so depth 0 is the identity and an empty list
    ;; survives until the layer that opens it is reached.  'deepest (and any
    ;; negative depth) flattens to the fixed point.
    (define (flatten-once l)
      (let loop ((lst l) (r '()))
        (cond ((null? lst) (reverse r))
              ((null? (car lst)) (loop (cdr lst) r))
              ((not (pair? (car lst))) (loop (cdr lst) (cons (car lst) r)))
              (else (loop (cdr lst) (append (reverse (car lst)) r))))))
    (define (flatten-all l)
      (let loop ((cur l))
        (let ((nxt (flatten-once cur)))
          (if (equal? nxt cur) nxt (loop nxt)))))
    (define (flatten-depth l d)
      (if (= d 0) l (flatten-depth (flatten-once l) (- d 1))))
    (define* (flatten lst (depth 1))
      (cond ((or (eq? depth 'deepest)
                 (and (integer? depth) (negative? depth)))
             (flatten-all lst))
            ((and (integer? depth) (>= depth 0))
             (flatten-depth lst depth))
            (else
             (type-error (string-append "flatten: depth should be 'deepest or integer, got ~A")
                         depth))))

  ) ;begin
) ;define-library
