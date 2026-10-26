;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions event)
  (import (scheme base)
    (liii error)
    (liii pi code core extensions types)
    (liii pi code core extensions registry)
  ) ;import
  (export pi-on! pi-emit!)
  (begin

    (define (normalize-event-type event-type)
      (cond ((symbol? event-type) event-type)
            ((string? event-type) (string->symbol event-type))
            (else (type-error "event-type must be a symbol or string"))
      ) ;cond
    ) ;define

    (define (pi-on! target event-type handler)
      (let* ((reg (target->registry target)) (evt-sym (normalize-event-type event-type)))
        (unless (procedure? handler)
          (type-error "handler must be a procedure")
        ) ;unless
        (let* ((listeners (extension-registry-listeners reg))
               (entry (assq evt-sym listeners))
              ) ;
          (if entry
            (set-cdr! entry (append (cdr entry) (list handler)))
            (set-extension-registry-listeners! reg (cons (list evt-sym handler) listeners))
          ) ;if
          #t
        ) ;let*
      ) ;let*
    ) ;define

    (define (call-listener-handler fn payload ctx)
      (let ((ar (if (procedure? fn) (car (arity fn)) 2)))
        (if (= ar 1) (fn payload) (fn payload ctx))
      ) ;let
    ) ;define

    (define (parse-terminate-flag ret)
      (cond
       ((and (list? ret) (assq 'terminate ret))
        (let ((val (cdr (assq 'terminate ret))))
          (if val #t #f)
        ) ;let
       ) ;
       ((and (pair? ret) (eq? (car ret) 'block) (pair? (cdr ret)))
        (let loop
          ((rest (cdr ret)))
          (cond ((null? rest) #f)
                ((and (pair? (car rest)) (eq? (caar rest) 'terminate)) (if (cdar rest) #t #f))
                ((and (eq? (car rest) :terminate) (pair? (cdr rest))) (if (cadr rest) #t #f))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (%parse-block-pair rest)
      (cond
       ((pair? rest)
        (if (pair? (car rest))
          (cond ((assq 'reason rest) => cdr)
                (else "Tool execution blocked by extension")
          ) ;cond
          (car rest)
        ) ;if
       ) ;
       ((string? rest) rest)
       (else "Tool execution blocked by extension")
      ) ;cond
    ) ;define

    (define (%extract-block-reason ret act)
      (cond
       ((assq 'reason ret)
        =>
        (lambda (p) (if (string? (cdr p)) (cdr p) (object->string (cdr p))))
       ) ;
       ((string? (cdr act)) (cdr act))
       (else "Tool execution blocked by extension")
      ) ;cond
    ) ;define

    (define (parse-block-ret ret)
      (cond
       ((and (pair? ret) (eq? (car ret) 'block)) (%parse-block-pair (cdr ret)))
       ((and (list? ret) (or (assq 'block ret) (assq 'action ret)))
        (let ((act (or (assq 'block ret) (assq 'action ret))))
          (if
            (or (and (eq? (car act) 'block) (cdr act))
              (and (eq? (car act) 'action) (eq? (cdr act) 'block))
            ) ;or
            (%extract-block-reason ret act)
            #f
          ) ;if
        ) ;let
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (parse-override-ret ret)
      (cond
       ((and (pair? ret) (eq? (car ret) 'override))
        (if (pair? (cdr ret)) (cadr ret) (cdr ret))
       ) ;
       ((and (list? ret) (assq 'override ret))
        (cond ((assq 'input ret) => cdr)
              ((assq 'params ret) => cdr)
              (else #f)
        ) ;cond
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (apply-input-override payload new-input)
      (let loop
        ((rest payload) (acc '()) (replaced #f))
        (if (null? rest)
          (if replaced (reverse acc) (cons (cons 'input new-input) (reverse acc)))
          (let ((pair (car rest)))
            (if (or (eq? (car pair) 'input) (eq? (car pair) 'params))
              (loop (cdr rest) (cons (cons 'input new-input) acc) #t)
              (loop (cdr rest) (cons pair acc) replaced)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%log-listener-error evt-sym key args)
      (let ((err-p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display (string-append "[WARN] extension listener error on "
                   (symbol->string evt-sym)
                   ": "
                 ) ;string-append
          err-p
        ) ;display
        (display key err-p)
        (display " " err-p)
        (display args err-p)
        (newline err-p)
      ) ;let
    ) ;define

    (define (%build-block-result block-reason invoked-val)
      (let ((term? (parse-terminate-flag invoked-val)))
        (list (cons 'action 'block)
          (cons 'reason
            (if (string? block-reason) block-reason "Tool execution blocked by extension")
          ) ;cons
          (cons 'terminate (if term? #t #f))
        ) ;list
      ) ;let
    ) ;define

    (define (%handle-handler-ret invoked-val current-payload handlers loop)
      (let ((block-reason (parse-block-ret invoked-val))
            (override-input (parse-override-ret invoked-val))
           ) ;
        (cond (block-reason (%build-block-result block-reason invoked-val))
              (override-input
                (let ((updated-payload (apply-input-override current-payload override-input)))
                  (loop (cdr handlers) updated-payload)
                ) ;let
              ) ;override-input
              (else (loop (cdr handlers) current-payload))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-emit! target event-type payload)
      (let* ((reg (target->registry target))
             (ctx (if (extension-context? target) target (make-extension-context :registry reg))
             ) ;ctx
             (evt-sym (normalize-event-type event-type))
             (listeners (extension-registry-get-listeners reg evt-sym))
            ) ;
        (let loop
          ((handlers listeners) (current-payload payload))
          (if (null? handlers)
            (list (cons 'action 'continue) (cons 'payload current-payload))
            (let* ((h (car handlers)) (invoked-val #f) (invoked-ok #t))
              (catch #t
                (lambda () (set! invoked-val (call-listener-handler h current-payload ctx)))
                (lambda (key . args)
                  (set! invoked-ok #f)
                  (%log-listener-error evt-sym key args)
                ) ;lambda
              ) ;catch
              (if (not invoked-ok)
                ;; 监听器出错时隔离，继续执行后续监听器
                (loop (cdr handlers) current-payload)
                (%handle-handler-ret invoked-val current-payload handlers loop)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
