;;; env.scm
;;; Environments map names (symbols) to values.
;;; An environment is a list of frames, where each frame is an
;;; association list ((name . value) ...).

(define (env-empty) '())

(define (env-lookup env name)
  (let loop ((frames env))
    (if (null? frames)
        #f
        (let ((entry (assoc name (car frames))))
          (if entry
              (cdr entry)
              (loop (cdr frames)))))))

(define (env-extend env name value)
  (if (null? env)
      (list (list (cons name value)))
      (cons (cons (cons name value) (car env)) (cdr env))))

;;; env-map-values : (value -> value) env -> env
;;; Map f over every bound value in every frame, preserving names/frames.
;;; Used to build the "unstopped" environment for local-expand.

(define (env-map-values f env)
  (map (lambda (frame)
         (map (lambda (entry) (cons (car entry) (f (cdr entry)))) frame))
       env))

(module-define! the-expander-library 'env-empty env-empty)
(module-define! the-expander-library 'env-lookup env-lookup)
(module-define! the-expander-library 'env-extend env-extend)
(module-define! the-expander-library 'env-map-values env-map-values)
