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
  (import (scheme base) (srfi srfi-1) (srfi srfi-13) (liii error))
  (begin

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

    (define (not-null-list? l) (and (pair? l) #t))
    (define list-null? null?)
    (define list-not-null? not-null-list?)

    (define* (flatten lst (depth 1))
      (define (flatten-iter rest depth res-node)
        (if (null? rest) res-node
            (let ((first (car rest)) (tail (cdr rest)))
              (cond ((pair? first)
                     (if (or (eq? depth 'deepest) (> depth 0))
                         (flatten-iter tail depth (flatten-iter first (if (eq? depth 'deepest) 'deepest (- depth 1)) res-node))
                         (begin (set-cdr! res-node (cons first '())) (flatten-iter tail depth (cdr res-node)))))
                    ((null? first) (flatten-iter tail depth res-node))
                    (else (set-cdr! res-node (cons first '())) (flatten-iter tail depth (cdr res-node)))))))
      (define (flatten-with depth)
        (let ((res (cons #f '()))) (flatten-iter lst depth res) (cdr res)))
      (cond ((or (eq? depth 'deepest) (integer? depth)) (flatten-with depth))
            (else (type-error (string-append "flatten: depth should be 'deepest or integer, got ~A") depth))))

  ) ;begin
) ;define-library
