;
; SRFI-117: Queues based on lists
;
; Based on the SRFI-117 reference implementation:
; Copyright (C) 2017 Alex Shinn
;
; SPDX-License-Identifier: BSD-3-Clause
;
; Adapted for Goldfish Scheme.
;

(define-library (srfi srfi-117)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi srfi-1) list-copy last-pair)
          (srfi srfi-9)
  ) ;import
  (export make-list-queue
          list-queue
          list-queue-copy
          list-queue-unfold
          list-queue-unfold-right

          list-queue?
          list-queue-empty?

          list-queue-front
          list-queue-back
          list-queue-list
          list-queue-first-last

          list-queue-add-front!
          list-queue-add-back!
          list-queue-remove-front!
          list-queue-remove-back!
          list-queue-remove-all!
          list-queue-set-list!
          list-queue-append
          list-queue-append!
          list-queue-concatenate

          list-queue-map
          list-queue-map!
          list-queue-for-each
  ) ;export
  (begin

    (define-record-type <list-queue>
      (raw-make-list-queue first last)
      list-queue?
      (first list-queue-first-pair set-list-queue-first-pair!)
      (last list-queue-last-pair set-list-queue-last-pair!)
    ) ;define-record-type

    (define (%ensure-list-queue who obj)
      (if (list-queue? obj)
          obj
          (error 'wrong-type-arg (string-append who ": expected list-queue") obj)
      ) ;if
    ) ;define

    (define (%ensure-pair-or-null who obj)
      (unless (or (pair? obj) (null? obj))
        (error 'wrong-type-arg (string-append who ": expected pair or empty list") obj)
      ) ;unless
    ) ;define

    (define (%ensure-not-empty who queue)
      (%ensure-list-queue who queue)
      (when (list-queue-empty? queue)
        (error 'out-of-range (string-append who ": empty list-queue"))
      ) ;when
    ) ;define

    (define (%validate-first-last who first last)
      (%ensure-pair-or-null who first)
      (%ensure-pair-or-null who last)
      (when (or (and (null? first) (pair? last))
                (and (pair? first) (null? last)))
        (error 'wrong-type-arg
               (string-append who ": invalid first/last pair combination")
               first
               last
        ) ;error
      ) ;when
    ) ;define

    (define (%penult-pair pairs)
      (let loop ((pairs pairs))
        (cond ((null? (cdr pairs)) '())
              ((null? (cddr pairs)) pairs)
              (else (loop (cdr pairs)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%map! proc pairs)
      (let loop ((pairs pairs))
        (when (pair? pairs)
          (set-car! pairs (proc (car pairs)))
          (loop (cdr pairs))
        ) ;when
      ) ;let
    ) ;define

    (define (%join! result queue)
      (let ((source (if (eq? result queue)
                        (list-queue-copy queue)
                        queue))
      ) ;let
        (unless (list-queue-empty? source)
          (if (list-queue-empty? result)
              (begin
                (set-list-queue-first-pair! result (list-queue-first-pair source))
                (set-list-queue-last-pair! result (list-queue-last-pair source))
              ) ;begin
              (begin
                (set-cdr! (list-queue-last-pair result)
                          (list-queue-first-pair source)
                ) ;set-cdr!
                (set-list-queue-last-pair! result (list-queue-last-pair source))
              ) ;begin
          ) ;if
          (when (eq? source queue)
            (set-list-queue-first-pair! queue '())
            (set-list-queue-last-pair! queue '())
          ) ;when
        ) ;unless
        result
    ) ;define
  ) ;begin

    (define make-list-queue
      (case-lambda
        ((first)
         (%ensure-pair-or-null "make-list-queue" first)
         (if (null? first)
             (raw-make-list-queue '() '())
             (raw-make-list-queue first (last-pair first))
         ) ;if
        ) ;
        ((first last)
         (%validate-first-last "make-list-queue" first last)
         (raw-make-list-queue first last)
        ) ;
      ) ;case-lambda
    ) ;define

    (define (list-queue . elements)
      (make-list-queue elements)
    ) ;define

    (define (list-queue-copy queue)
      (%ensure-list-queue "list-queue-copy" queue)
      (make-list-queue (list-copy (list-queue-first-pair queue)))
    ) ;define

    (define list-queue-unfold
      (case-lambda
        ((stop? mapper successor seed)
         (list-queue-unfold stop? mapper successor seed (list-queue))
        ) ;
        ((stop? mapper successor seed queue)
         (%ensure-list-queue "list-queue-unfold" queue)
         (let loop ((seed seed))
           (if (stop? seed)
               queue
               (begin
                 (list-queue-add-front! (loop (successor seed)) (mapper seed))
                 queue
               ) ;begin
           ) ;if
         ) ;let
        ) ;
      ) ;case-lambda
    ) ;define

    (define list-queue-unfold-right
      (case-lambda
        ((stop? mapper successor seed)
         (list-queue-unfold-right stop? mapper successor seed (list-queue))
        ) ;
        ((stop? mapper successor seed queue)
         (%ensure-list-queue "list-queue-unfold-right" queue)
         (let loop ((seed seed))
           (if (stop? seed)
               queue
               (begin
                 (list-queue-add-back! (loop (successor seed)) (mapper seed))
                 queue
               ) ;begin
           ) ;if
         ) ;let
        ) ;
      ) ;case-lambda
    ) ;define

    (define (list-queue-empty? queue)
      (%ensure-list-queue "list-queue-empty?" queue)
      (null? (list-queue-first-pair queue))
    ) ;define

    (define (list-queue-front queue)
      (%ensure-not-empty "list-queue-front" queue)
      (car (list-queue-first-pair queue))
    ) ;define

    (define (list-queue-back queue)
      (%ensure-not-empty "list-queue-back" queue)
      (car (list-queue-last-pair queue))
    ) ;define

    (define (list-queue-list queue)
      (%ensure-list-queue "list-queue-list" queue)
      (list-queue-first-pair queue)
    ) ;define

    (define (list-queue-first-last queue)
      (%ensure-list-queue "list-queue-first-last" queue)
      (values (list-queue-first-pair queue)
              (list-queue-last-pair queue)
      ) ;values
    ) ;define

    (define (list-queue-add-front! queue element)
      (%ensure-list-queue "list-queue-add-front!" queue)
      (let* ((old-first (list-queue-first-pair queue))
             (new-first (cons element old-first))
      ) ;let*
        (when (null? old-first)
          (set-list-queue-last-pair! queue new-first)
        ) ;when
        (set-list-queue-first-pair! queue new-first)
      ) ;let*
    ) ;define

    (define (list-queue-add-back! queue element)
      (%ensure-list-queue "list-queue-add-back!" queue)
      (let ((new-last (list element)))
        (if (list-queue-empty? queue)
            (set-list-queue-first-pair! queue new-last)
            (set-cdr! (list-queue-last-pair queue) new-last)
        ) ;if
        (set-list-queue-last-pair! queue new-last)
      ) ;let
    ) ;define

    (define (list-queue-remove-front! queue)
      (%ensure-not-empty "list-queue-remove-front!" queue)
      (let* ((old-first (list-queue-first-pair queue))
             (element (car old-first))
             (new-first (cdr old-first))
      ) ;let*
        (when (null? new-first)
          (set-list-queue-last-pair! queue '())
        ) ;when
        (set-list-queue-first-pair! queue new-first)
        element
      ) ;let*
    ) ;define

    (define (list-queue-remove-back! queue)
      (%ensure-not-empty "list-queue-remove-back!" queue)
      (let* ((old-last (list-queue-last-pair queue))
             (element (car old-last))
             (new-last (%penult-pair (list-queue-first-pair queue)))
      ) ;let*
        (if (null? new-last)
            (set-list-queue-first-pair! queue '())
            (set-cdr! new-last '())
        ) ;if
        (set-list-queue-last-pair! queue new-last)
        element
      ) ;let*
    ) ;define

    (define (list-queue-remove-all! queue)
      (%ensure-list-queue "list-queue-remove-all!" queue)
      (let ((result (list-queue-first-pair queue)))
        (set-list-queue-first-pair! queue '())
        (set-list-queue-last-pair! queue '())
        result
      ) ;let
    ) ;define

    (define list-queue-set-list!
      (case-lambda
        ((queue first)
         (%ensure-list-queue "list-queue-set-list!" queue)
         (%ensure-pair-or-null "list-queue-set-list!" first)
         (set-list-queue-first-pair! queue first)
         (if (null? first)
             (set-list-queue-last-pair! queue '())
             (set-list-queue-last-pair! queue (last-pair first))
         ) ;if
        ) ;
        ((queue first last)
         (%ensure-list-queue "list-queue-set-list!" queue)
         (%validate-first-last "list-queue-set-list!" first last)
         (set-list-queue-first-pair! queue first)
         (set-list-queue-last-pair! queue last)
        ) ;
      ) ;case-lambda
    ) ;define

    (define (list-queue-append . queues)
      (list-queue-concatenate queues)
    ) ;define

    (define list-queue-append!
      (case-lambda
        (() (list-queue))
        ((queue)
         (%ensure-list-queue "list-queue-append!" queue)
         queue
        ) ;
        (queues
         (for-each
          (lambda (queue)
            (%ensure-list-queue "list-queue-append!" queue)
          ) ;lambda
          queues
         ) ;for-each
         (let ((result (car queues)))
           (for-each
            (lambda (queue)
              (%join! result queue)
            ) ;lambda
            (cdr queues)
           ) ;for-each
           result
         ) ;let
        ) ;queues
      ) ;case-lambda
    ) ;define

    (define (list-queue-concatenate queues)
      (let loop ((queues queues)
                 (result (list-queue))
      ) ;let
        (if (null? queues)
            result
            (begin
              (%ensure-list-queue "list-queue-concatenate" (car queues))
              (list-queue-for-each
               (lambda (element)
                 (list-queue-add-back! result element)
               ) ;lambda
               (car queues)
              ) ;list-queue-for-each
              (loop (cdr queues) result)
            ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (list-queue-map proc queue)
      (%ensure-list-queue "list-queue-map" queue)
      (make-list-queue (map proc (list-queue-first-pair queue)))
    ) ;define

    (define (list-queue-map! proc queue)
      (%ensure-list-queue "list-queue-map!" queue)
      (%map! proc (list-queue-first-pair queue))
    ) ;define

    (define (list-queue-for-each proc queue)
      (%ensure-list-queue "list-queue-for-each" queue)
      (for-each proc (list-queue-first-pair queue))
    ) ;define

  ) ;begin
) ;define-library
