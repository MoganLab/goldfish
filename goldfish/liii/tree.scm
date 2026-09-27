(define-library (liii tree)
  (import (goldfish))
  (export tree-cyclic? tree-leaves tree-memq tree-set-memq tree-count)
  (import (scheme base))
  (begin
    (define (tree-cyclic? tree)
      (letrec ((walk
                (lambda (node path)
                  (if (pair? node)
                      (if (memq node path)
                          #t
                          (or (walk (car node) (cons node path))
                              (walk (cdr node) (cons node path))))
                      #f))))
        (walk tree '())))

    (define (tree-leaves tree)
      (letrec ((walk
                (lambda (node path)
                  (if (pair? node)
                      (if (memq node path)
                          0
                          (+ (walk (car node) (cons node path))
                             (walk (cdr node) (cons node path))))
                      (if (null? node) 0 1)))))
        (walk tree '())))

    (define (tree-memq obj tree)
      (letrec ((walk
                (lambda (node path)
                  (if (pair? node)
                      (if (memq node path)
                          #f
                          (or (eq? obj (car node))
                              (walk (car node) (cons node path))
                              (walk (cdr node) (cons node path))))
                      (eq? obj node)))))
        (walk tree '())))

    (define (tree-set-memq symbols tree)
      (letrec ((walk
                (lambda (node path)
                  (if (pair? node)
                      (if (memq node path)
                          #f
                          (or (if (memq (car node) symbols) #t #f)
                              (walk (car node) (cons node path))
                              (walk (cdr node) (cons node path))))
                      (if (memq node symbols) #t #f)))))
        (walk tree '())))

    (define (tree-count obj tree . max-count-arg)
      (let ((max-count (if (null? max-count-arg) #f (car max-count-arg))))
        (letrec ((walk
                   (lambda (node path count)
                     (if (and max-count (>= count max-count))
                         count
                         (if (pair? node)
                             (if (memq node path)
                                 count
                                 (walk (cdr node) (cons node path)
                                       (walk (car node) (cons node path)
                                             count)))
                             (if (eq? obj node) (+ count 1) count))))))
          (walk tree '() 0)))))
) ;define-library
