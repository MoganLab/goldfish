(define-library (liii goldfix-edit)
  (export apply-edits)
  (import (liii base)
          (liii goldfix-record))

  (begin
    (define (edit-position edit)
      (if (eq? (fix-edit-kind edit) 'insert)
          (fix-edit-offset edit)
          (fix-edit-start edit)))

    (define (delete-before-insert? edit-a edit-b)
      (and (eq? (fix-edit-kind edit-a) 'delete)
           (eq? (fix-edit-kind edit-b) 'insert)))

    (define (edit-before? edit-a edit-b)
      (let ((pos-a (edit-position edit-a))
            (pos-b (edit-position edit-b)))
        (cond
          ((> pos-a pos-b) #t)
          ((< pos-a pos-b) #f)
          ((delete-before-insert? edit-a edit-b) #t)
          (else #f))))

    (define (insert-edit-sorted edit sorted)
      (cond
        ((null? sorted) (list edit))
        ((edit-before? edit (car sorted))
         (cons edit sorted))
        (else
          (cons (car sorted)
                (insert-edit-sorted edit (cdr sorted))))))

    (define (sort-edits edits)
      (let loop ((rest edits)
                 (result '()))
        (if (null? rest)
            result
            (loop (cdr rest)
                  (insert-edit-sorted (car rest) result)))))

    (define (delete-range source start end)
      (string-append (substring source 0 start)
                     (substring source end (string-length source))))

    (define (insert-text source offset text)
      (string-append (substring source 0 offset)
                     text
                     (substring source offset (string-length source))))

    (define (apply-one-edit source edit)
      (if (eq? (fix-edit-kind edit) 'delete)
          (delete-range source (fix-edit-start edit) (fix-edit-end edit))
          (insert-text source (fix-edit-offset edit) (fix-edit-text edit))))

    (define (apply-edits source edits)
      (let loop ((rest (sort-edits edits))
                 (result source))
        (if (null? rest)
            result
            (loop (cdr rest)
                  (apply-one-edit result (car rest))))))
  ) ;begin
) ;define-library
