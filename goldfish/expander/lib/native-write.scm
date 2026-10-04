;; Install public writers after the source reader and its graph traversal.
;; Scheme owns graph semantics; the engine emits atomic representations.
(define (native-write-to-port obj ports mode)
  (if (and (pair? ports) (pair? (cdr ports)))
    (error 'wrong-number-of-args "writer accepts one optional port")
    (write-roundtrip obj
                     (if (null? ports) (current-output-port) (car ports))
                     mode)))

(define (write obj . ports) (native-write-to-port obj ports 'write))
(define (write-shared obj . ports) (native-write-to-port obj ports 'shared))
(define (write-simple obj . ports) (native-write-to-port obj ports 'simple))
(define (display obj . ports) (native-write-to-port obj ports 'display))
