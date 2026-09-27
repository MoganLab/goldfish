(define-library (liii vector)
  (import (goldfish))
  (import (scheme base) (liii error) (srfi srfi-133) (srfi srfi-13))
  (export vector-empty?
    vector-unfold
    vector-unfold-right
    vector-unfold!
    vector-unfold-right!
    vector-fold
    vector-fold-right
    vector-count
    vector-any
    vector-every
    vector-index
    vector-index-right
    vector-skip
    vector-skip-right
    vector-binary-search
    vector-concatenate
    vector-partition
    vector-append-subvectors
    vector-swap!
    vector-reverse!
    vector-reverse-copy
    vector-reverse-copy!
    vector-map!
    vector-cumulate
    reverse-vector->list
    reverse-list->vector
    vector=
    vector-contains?
    vector-filter
    vector-contains?
    vector-take
    vector-drop
    vector-take-right
    vector-drop-right
    fill!
    int-vector
    int-vector?
    make-int-vector
    int-vector-ref
    int-vector-set!
    complex-vector
    complex-vector?
    make-complex-vector
    complex-vector-ref
    complex-vector-set!
    float-vector
    float-vector?
    make-float-vector
    float-vector-ref
    float-vector-set!
  ) ;export
  (begin

    (define (vector-filter pred vec)
      (unless (procedure? pred)
        (error 'wrong-type-arg "vector-filter: expected a procedure" pred))
      (unless (vector? vec)
        (error 'wrong-type-arg "vector-filter: expected a vector" vec))
      (let ((result (make-vector (vector-count pred vec))))
        (let loop ((i 0) (j 0))
          (if (= i (vector-length vec))
              result
              (let ((value (vector-ref vec i)))
                (if (pred value)
                    (begin
                      (vector-set! result j value)
                      (loop (+ i 1) (+ j 1)))
                    (loop (+ i 1) j)))))))

    (define (fill! vec value . range)
      (apply vector-fill! vec value range))

    ;; Keep the native implementation portable while preserving the
    ;; distinction between integer and ordinary vectors.
    (define *int-vectors* '())

    (define (int-vector? obj)
      (if (assq obj *int-vectors*) #t #f))

    (define (register-int-vector! vec)
      (set! *int-vectors* (cons (cons vec #t) *int-vectors*))
      vec)

    (define (int-vector . values)
      (for-each
        (lambda (value)
          (unless (integer? value)
            (error 'wrong-type-arg "int-vector: expected integer" value)))
        values)
      (register-int-vector! (list->vector values)))

    (define (make-int-vector len . fill-value)
      (unless (and (integer? len) (exact? len) (>= len 0))
        (error 'wrong-type-arg "make-int-vector: invalid length" len))
      (when (> (length fill-value) 1)
        (error 'wrong-number-of-args "make-int-vector: expected one or two arguments"))
      (let ((fill (if (null? fill-value) 0 (car fill-value))))
        (unless (integer? fill)
          (error 'wrong-type-arg "make-int-vector: expected integer fill" fill))
        (register-int-vector! (make-vector len fill))))

    (define (int-vector-ref vec index)
      (unless (int-vector? vec)
        (error 'wrong-type-arg "int-vector-ref: expected int-vector" vec))
      (unless (and (integer? index) (exact? index)
                   (>= index 0) (< index (vector-length vec)))
        (error 'out-of-range "int-vector-ref: index out of range" index))
      (vector-ref vec index))

    (define (int-vector-set! vec index value)
      (unless (int-vector? vec)
        (error 'wrong-type-arg "int-vector-set!: expected int-vector" vec))
      (unless (and (integer? index) (exact? index)
                   (>= index 0) (< index (vector-length vec)))
        (error 'out-of-range "int-vector-set!: index out of range" index))
      (unless (integer? value)
        (error 'wrong-type-arg "int-vector-set!: expected integer" value))
      (vector-set! vec index value))

    (define (vector-contains? vec elem . args)
      (let ((cmp (if (null? args) equal? (car args))))
        (not (not (vector-index (lambda (x) (cmp x elem)) vec)))
      ) ;let
    ) ;define

    (define (vector-take vec n)
      (unless (vector? vec)
        (type-error "vector-take: first argument must be a vector" vec)
      ) ;unless
      (unless (integer? n)
        (type-error "vector-take: second argument must be an integer" n)
      ) ;unless
      (let ((len (vector-length vec)))
        (cond ((< n 0) (vector))
              ((>= n len) vec)
              (else (vector-copy vec 0 n))
        ) ;cond
      ) ;let
    ) ;define

    (define (vector-drop vec n)
      (unless (vector? vec)
        (type-error "vector-drop: first argument must be a vector" vec)
      ) ;unless
      (unless (integer? n)
        (type-error "vector-drop: second argument must be an integer" n)
      ) ;unless
      (let ((len (vector-length vec)))
        (cond ((< n 0) vec)
              ((>= n len) (vector))
              (else (vector-copy vec n))
        ) ;cond
      ) ;let
    ) ;define

    (define (vector-take-right vec n)
      (unless (vector? vec)
        (type-error "vector-take-right: first argument must be a vector" vec)
      ) ;unless
      (unless (integer? n)
        (type-error "vector-take-right: second argument must be an integer" n)
      ) ;unless
      (let ((len (vector-length vec)))
        (cond ((< n 0) (vector))
              ((>= n len) vec)
              (else (vector-copy vec (- len n)))
        ) ;cond
      ) ;let
    ) ;define

    (define (vector-drop-right vec n)
      (unless (vector? vec)
        (type-error "vector-drop-right: first argument must be a vector" vec)
      ) ;unless
      (unless (integer? n)
        (type-error "vector-drop-right: second argument must be an integer" n)
      ) ;unless
      (let ((len (vector-length vec)))
        (cond ((< n 0) vec)
              ((>= n len) (vector))
              (else (vector-copy vec 0 (- len n)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
