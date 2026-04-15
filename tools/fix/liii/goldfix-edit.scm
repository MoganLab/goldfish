(define-library (liii goldfix-edit)
  (export apply-edits)
  (import (liii base)
    (liii goldfix-record)
  ) ;import

  (begin
    (define (edit-position edit)
      (if (eq? (fix-edit-kind edit) 'insert)
        (fix-edit-offset edit)
        (fix-edit-start edit)
      ) ;if
    ) ;define

    (define (delete-before-insert? edit-a edit-b)
      (and (eq? (fix-edit-kind edit-a) 'delete)
        (eq? (fix-edit-kind edit-b) 'insert)
      ) ;and
    ) ;define

    (define (edit-before? edit-a edit-b)
      (let ((pos-a (edit-position edit-a))
            (pos-b (edit-position edit-b))
           ) ;
        (cond ((> pos-a pos-b) #t)
              ((< pos-a pos-b) #f)
              ((delete-before-insert? edit-a edit-b)
               #t
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (insert-edit-sorted edit sorted)
      (cond ((null? sorted) (list edit))
            ((edit-before? edit (car sorted))
             (cons edit sorted)
            ) ;
            (else (cons (car sorted)
                    (insert-edit-sorted edit (cdr sorted))
                  ) ;cons
            ) ;else
      ) ;cond
    ) ;define

    (define (sort-edits edits)
      (let loop
        ((rest edits) (result '()))
        (if (null? rest)
          result
          (loop (cdr rest)
            (insert-edit-sorted (car rest) result)
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (delete-range source start end)
      (string-append (substring source 0 start)
        (substring source
          end
          (string-length source)
        ) ;substring
      ) ;string-append
    ) ;define

    (define (insert-text source offset text)
      (string-append (substring source 0 offset)
        text
        (substring source
          offset
          (string-length source)
        ) ;substring
      ) ;string-append
    ) ;define

    (define (apply-one-edit source edit)
      (if (eq? (fix-edit-kind edit) 'delete)
        (delete-range source
          (fix-edit-start edit)
          (fix-edit-end edit)
        ) ;delete-range
        (insert-text source
          (fix-edit-offset edit)
          (fix-edit-text edit)
        ) ;insert-text
      ) ;if
    ) ;define

    (define (apply-edits source edits)
      (let loop
        ((rest (sort-edits edits))
         (result source)
        ) ;
        (if (null? rest)
          result
          (loop (cdr rest)
            (apply-one-edit result (car rest))
          ) ;loop
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
