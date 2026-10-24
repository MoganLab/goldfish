;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension loader)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code extension types)
  ) ;import
  (export pi-load-extension pi-load-extensions pi-load-results-success-count
    pi-load-results-failed-count
  ) ;export
  (begin

    (define (%eval-extension-file path-str err-p)
      (let ((load-ok #t) (load-val #f) (err-msg #f))
        (catch #t
          (lambda () (set! load-val (load path-str)))
          (lambda (key . args)
            (set! load-ok #f)
            (set! err-msg (string-append (object->string key) ": " (object->string args)))
            (display (string-append "[WARN] failed to load extension " path-str ": ") err-p)
            (display key err-p)
            (display " " err-p)
            (display args err-p)
            (newline err-p)
          ) ;lambda
        ) ;catch
        (if load-ok (cons 'ok load-val) (cons 'error err-msg))
      ) ;let
    ) ;define

    (define (%call-factory-safely factory api-obj ctx)
      (if (and ctx (procedure? factory))
        (catch #t (lambda () (factory api-obj ctx)) (lambda args (factory api-obj)))
        (factory api-obj)
      ) ;if
    ) ;define

    (define (%invoke-factory factory api-obj ctx path-str err-p)
      (let ((invoke-ok #t) (invoke-res #f) (invoke-err #f))
        (catch #t
          (lambda () (set! invoke-res (%call-factory-safely factory api-obj ctx)))
          (lambda (key . args)
            (set! invoke-ok #f)
            (set! invoke-err
              (string-append (object->string key) ": " (object->string args))
            ) ;set!
            (display (string-append "[WARN] extension factory error in " path-str ": ")
              err-p
            ) ;display
            (display key err-p)
            (display " " err-p)
            (display args err-p)
            (newline err-p)
          ) ;lambda
        ) ;catch
        (if invoke-ok
          (list (cons 'status 'ok) (cons 'path path-str) (cons 'value invoke-res))
          (list (cons 'status 'error) (cons 'path path-str) (cons 'error invoke-err))
        ) ;if
      ) ;let
    ) ;define

    (define (pi-load-extension path-or-entry api . opt-context)
      (let* ((ctx (if (pair? opt-context) (car opt-context) #f))
             (api-obj (if (extension-api? api) api (make-extension-api api)))
             (p (if (path? path-or-entry) path-or-entry (path path-or-entry)))
             (path-str (path->string (path-resolve p)))
             (err-p (if (defined? '*stderr*) *stderr* (current-error-port)))
            ) ;
        (if (not (and (path-exists? p) (path-file? p)))
          (begin
            (display (string-append "[WARN] failed to load extension " path-str ": file not found\n")
              err-p
            ) ;display
            (list (cons 'status 'error)
              (cons 'path path-str)
              (cons 'error "file not found")
            ) ;list
          ) ;begin
          (let ((eval-res (%eval-extension-file path-str err-p)))
            (if (eq? (car eval-res) 'error)
              (list (cons 'status 'error) (cons 'path path-str) (cons 'error (cdr eval-res)))
              (let ((val (cdr eval-res)))
                (if (procedure? val)
                  (%invoke-factory val api-obj ctx path-str err-p)
                  (list (cons 'status 'ok) (cons 'path path-str) (cons 'value val))
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-load-extensions paths api . opt-context)
      (let* ((ctx (if (pair? opt-context) (car opt-context) #f))
             (results
               (map (if ctx
                      (lambda (p) (pi-load-extension p api ctx))
                      (lambda (p) (pi-load-extension p api))
                    ) ;if
                 (if (list? paths) paths '())
               ) ;map
             ) ;results
             (total (length results))
             (success
               (count
                 (lambda (r) (eq? (cdr (assq 'status r)) 'ok))
                 results
               ) ;count
             ) ;success
             (failed (- total success))
            ) ;
        (list (cons 'total total)
          (cons 'success success)
          (cons 'failed failed)
          (cons 'results results)
        ) ;list
      ) ;let*
    ) ;define

    (define (pi-load-results-success-count report)
      (if (pair? report) (cond ((assq 'success report) => cdr) (else 0)) 0)
    ) ;define

    (define (pi-load-results-failed-count report)
      (if (pair? report) (cond ((assq 'failed report) => cdr) (else 0)) 0)
    ) ;define

  ) ;begin
) ;define-library
