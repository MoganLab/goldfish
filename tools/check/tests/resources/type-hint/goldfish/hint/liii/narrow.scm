(define-library (hint liii narrow)
  (import (scheme base))
  (export hint-liii/narrow)
  (begin

    ;; guarded-if : (any? -> any?)
    (define hint-guarded-if '((any? -> any?)))

    ;; guarded-if2 : (any? -> any?)
    (define hint-guarded-if2 '((any? -> any?)))

    ;; guarded-and : (any? -> any?)
    (define hint-guarded-and '((any? -> any?)))

    ;; guarded-and2 : (any? -> any?)
    (define hint-guarded-and2 '((any? -> any?)))

    ;; guarded-cond : (any? -> any?)
    (define hint-guarded-cond '((any? -> any?)))

    ;; unguarded : (any? -> any?)
    (define hint-unguarded '((any? -> any?)))

    (define (hint-liii/narrow name)
      (case name
       ((guarded-if) hint-guarded-if)
       ((guarded-if2) hint-guarded-if2)
       ((guarded-and) hint-guarded-and)
       ((guarded-and2) hint-guarded-and2)
       ((guarded-cond) hint-guarded-cond)
       ((unguarded) hint-unguarded)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
