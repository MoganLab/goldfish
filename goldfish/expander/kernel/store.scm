;;; store.scm
;;; The expand-time store holds:
;;;   counter  -- for generating unique names/scopes/boxes/def-envs
;;;   bindings -- symbol -> (list (Bind phase scopes name) ...)
;;;   boxes    -- address -> value
;;;   def-envs -- address -> env

(define-record-type <bind>
  (make-bind phase scopes name)
  bind?
  (phase bind-phase)
  (scopes bind-scopes)
  (name bind-name))

(define-record-type <store>
  (make-store counter bindings boxes def-envs)
  store?
  (counter store-counter)
  (bindings store-bindings)
  (boxes store-boxes)
  (def-envs store-def-envs))

(define-public (store-empty) (make-store 0 '() '() '()))

;;; Allocation

(define (store-alloc store prefix)
  (let ((counter (store-counter store)))
    (values (string->symbol (format #f "~A:~A" prefix counter))
            (make-store (+ 1 counter)
                        (store-bindings store)
                        (store-boxes store)
                        (store-def-envs store)))))

(define-public (store-alloc-name store id)
  (store-alloc store (syntax-form id)))

(define-public (store-alloc-scope store)
  (store-alloc store 'scp))

(define-public (store-alloc-box store)
  (store-alloc store 'box))

(define-public (store-alloc-def-env store)
  (store-alloc store 'def-env))

;;; Binding resolution

(define (store-lookup store sym)
  (let ((entry (assoc sym (store-bindings store))))
    (if entry (cdr entry) '())))

(define (biggest-subset scps scps-set)
  (let ((matching (sort (lambda (a b) (> (length a) (length b)))
                        (filter (lambda (s) (set<=? s scps)) scps-set))))
    (cond
      ((null? matching) #f)
      ;; Two bindings with EXACTLY the same scope set are redefinitions
      ;; (e.g. duplicate internal defines, which s7 lets the last one
      ;; shadow): not an ambiguity.  store-bind conses newer bindings
      ;; first, so store-resolve's linear search picks the last define.
      ((and (pair? (cdr matching))
            (set=? (car matching) (cadr matching)))
       (car matching))
      ((and (pair? (cdr matching))
            (= (length (car matching)) (length (cadr matching))))
       #f)
      ((any (lambda (b) (not (set<=? b (car matching)))) (cdr matching))
       #f)
      (else (car matching)))))

(define-public (store-bind store phase id name)
  (let* ((sym (syntax-form id))
         (scopes (stx-ctx-at (syntax-context id) phase))
         (bind (make-bind phase scopes name))
         (bindings (store-bindings store))
         (entry (assoc sym bindings)))
    (make-store (store-counter store)
                (if entry
                    (map (lambda (b)
                           (if (eq? (car b) sym)
                               (cons sym (cons bind (cdr b)))
                               b))
                         bindings)
                    (cons (list sym bind) bindings))
                (store-boxes store)
                (store-def-envs store))))

(define-public (store-resolve store phase id)
  (let* ((sym (syntax-form id))
         (scopes (stx-ctx-at (syntax-context id) phase))
         (binds (filter (lambda (b) (= (bind-phase b) phase))
                        (store-lookup store sym))))
    (if (null? binds)
        sym
        (let ((biggest (biggest-subset scopes (map bind-scopes binds))))
          (if biggest
              (let loop ((bs binds))
                (if (null? bs)
                    sym
                    (if (set=? (bind-scopes (car bs)) biggest)
                        (bind-name (car bs))
                        (loop (cdr bs)))))
              (if (any (lambda (b) (set<=? (bind-scopes b) scopes)) binds)
                  (error "ambiguous reference" sym)
                  sym))))))

;;; Mutable boxes (used at expand-time, e.g. by defs model)

(define-public (store-box-ref store addr)
  (let ((entry (assoc addr (store-boxes store))))
    (if entry
        (cdr entry)
        (error "store-box-ref: unbound box" addr))))

(define-public (store-box-set store addr value)
  (let ((boxes (store-boxes store)))
    (make-store (store-counter store)
                (store-bindings store)
                (if (assoc addr boxes)
                    (map (lambda (b)
                           (if (eq? (car b) addr)
                               (cons addr value)
                               b))
                         boxes)
                    (cons (cons addr value) boxes))
                (store-def-envs store))))

;;; Definition-context environments

(define-public (store-def-env-ref store addr)
  (let ((entry (assoc addr (store-def-envs store))))
    (if entry
        (cdr entry)
        (error "store-def-env-ref: unbound def-env" addr))))

(define-public (store-def-env-set store addr env)
  (let ((def-envs (store-def-envs store)))
    (make-store (store-counter store)
                (store-bindings store)
                (store-boxes store)
                (if (assoc addr def-envs)
                    (map (lambda (e)
                           (if (eq? (car e) addr)
                               (cons addr env)
                               e))
                         def-envs)
                    (cons (cons addr env) def-envs)))))

;;; Library exports

(module-define! the-expander-library 'make-store make-store)
(module-define! the-expander-library 'store? store?)
(module-define! the-expander-library 'store-counter store-counter)
(module-define! the-expander-library 'store-bindings store-bindings)
(module-define! the-expander-library 'store-boxes store-boxes)
(module-define! the-expander-library 'store-def-envs store-def-envs)
