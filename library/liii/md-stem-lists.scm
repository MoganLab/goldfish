
(define-library (liii md-stem-lists)
  (import (scheme base))
  (import (liii utils markdown-list-util))
  (export make-bullet-list-stem)
  (export make-ordered-list-stem)
  (export make-list-item-stem)
  (begin
    (define (list-item-task-status lines)
      (and (pair? lines) (task-list-item-status (car lines)))
    ) ;define

    (define (strip-task-list-item-lines lines)
      (if (null? lines)
        '()
        (cons (strip-task-list-item-marker (car lines)) (cdr lines))
      ) ;if
    ) ;define

    (define (make-bullet-list-stem tightness item-lines-list parse-blocks)
      (cons 'bullet-list
        (cons tightness
          (map (lambda (lines) (make-list-item-stem lines parse-blocks)) item-lines-list)
        ) ;cons
      ) ;cons
    ) ;define

    (define (make-ordered-list-stem start delimiter tightness item-lines-list parse-blocks)
      (cons 'ordered-list
        (cons start
          (cons delimiter
            (cons tightness
              (map (lambda (lines) (make-list-item-stem lines parse-blocks)) item-lines-list)
            ) ;cons
          ) ;cons
        ) ;cons
      ) ;cons
    ) ;define

    (define (make-list-item-stem lines parse-blocks)
      (let ((task-status (list-item-task-status lines)))
        (if task-status
          (cons 'item
            (cons task-status (parse-blocks (strip-task-list-item-lines lines)))
          ) ;cons
          (cons 'item (parse-blocks lines))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
