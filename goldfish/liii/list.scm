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
      (define (flatten-depth-iter rest depth res-node)
        (if (null? rest)
          res-node
          (let ((first (car rest)) (tail (cdr rest)))
            (cond ((and (null? first) (not (= 0 depth))) (flatten-depth-iter tail depth res-node))
                  ((or (= depth 0) (not (pair? first)))
                   (set-cdr! res-node (cons first '()))
                   (flatten-depth-iter tail depth (cdr res-node))
                  ) ;
                  (else (flatten-depth-iter tail depth (flatten-depth-iter first (- depth 1) res-node))
                  ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;define
      (define (flatten-depth lst depth)
        (let ((res (cons #f '())))
          (flatten-depth-iter lst depth res)
          (cdr res)
        ) ;let
      ) ;define

      (define (flatten-deepest-iter rest res-node)
        (if (null? rest)
          res-node
          (let ((first (car rest)) (tail (cdr rest)))
            (cond ((pair? first)
                   (flatten-deepest-iter tail (flatten-deepest-iter first res-node))
                  ) ;
                  ((null? first) (flatten-deepest-iter tail res-node))
                  (else (set-cdr! res-node (cons first '()))
                    (flatten-deepest-iter tail (cdr res-node))
                  ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;define
      (define (flatten-deepest lst)
        (let ((res (cons #f '())))
          (flatten-deepest-iter lst res)
          (cdr res)
        ) ;let
      ) ;define

      (cond ((eq? depth 'deepest) (flatten-deepest lst))
            ((integer? depth) (flatten-depth lst depth))
            (else (type-error (string-append "flatten: the second argument depth should be symbol "
                                "`deepest' or a integer, which will be uesd as depth,"
                                " but got a ~A"
                              ) ;string-append
                    depth
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define*

  ) ;begin
) ;define-library
