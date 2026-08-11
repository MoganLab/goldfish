;; 0-clause BSD by Bill Schottstaedt from S7 source repo: s7test.scm
(define-library (srfi srfi-2)
  (export and-let*)
  (begin

    (define-syntax and-let*
      (syntax-rules ()
        ((and-let* () body ...)
         (begin body ...))
        ((and-let* ((var val) more ...) body ...)
         (let ((var val))
           (and var (and-let* (more ...) body ...))))
        ((and-let* (val) body ...)
         (let ((t val)) (and t (begin body ...))))
        ((and-let* (val more ...) body ...)
         (let ((t val)) (and t (and-let* (more ...) body ...))))))

  ) ;begin
) ;define-library
