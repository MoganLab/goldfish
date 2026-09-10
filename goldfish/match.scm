;;; match.scm -- SRFI-262: extensible pattern matching.
;;;
;;; A self-hosted implementation of SRFI-262, following the
;;; Racket-style extension mechanism: a pattern syntax keyword is
;;; bound to a *transformer* via define-pattern-syntax; at expansion
;;; time the matcher resolves the keyword's binding with
;;; syntax-local-value and calls the transformer to rewrite the
;;; pattern into core primitives.
;;;
;;; Implemented patterns (SRFI-262):
;;;   _  id  datum  (quote d)
;;;   (list ...)  (cons a d)  (cons* ...)  (vector ...)  (lset ...)
;;;   (eof-object)
;;;   (and ...)  (or ...)  (not ...)  (? proc subpat ...)  (=> proc subpat ...)
;;;   (seq ...)  (seq* ...)  (seq/unordered ...)   (quasiquote ...)
;;;   ellipsis (...) in sequence patterns, implemented with an NFA
;;;   (Pike/Laurikari) simulation
;;;
;;; Code generation: each clause compiles to nested if/let code
;;;
;;;   (if guard (let ((v ...)) body) <next-clause-code>)
;;;
;;; where the failure branch of every guard is the code of the
;;; following clause (or a match violation raise for the last).
;;; Sequence patterns (seq/seq*/seq/unordered and their derived
;;; forms) are compiled to a small NFA virtual machine.

(define-library (goldfish match)
  (import (goldfish))
  (import (goldfish match expansion))
  (import (scheme base)
          (scheme case-lambda))
  (export match
    case-lambda
    match-lambda
    match-values
    match-let
    match-let*
    match-let-values
    match-let*-values
    match-define
    match-define-values
    match-letrec
    match-letrec*
    if-match
    define-pattern-syntax
    match-ellipsis?
    &match
    make-match-violation
    match-violation?
    %make-registers
    %registers-set!
    %register-ref
    %registers-cow!
    %make-instruction
    %instruction-type
    %instruction-arg0
    %instruction-arg1
    %make-vm
    %vm-step!
    %vm-alive?
    %vm-destroy!
    %vm-finished-thread
    %vm-prune-nonfinal-threads!
    %thread-registers
    %bitwise-bit-set?
    %bitwise-bit-set)
  (begin

    ;; ------------------------------------------------------------------
    ;; Match violation (SRFI-262 condition).

    (define-record-type &match
      (%make-match-condition irritants)
      &match?
      (irritants match-condition-irritants))

    ;; SRFI-262 violation condition: the generated fail paths raise it
    ;; without irritants; user code may attach them.
    (define (make-match-violation . irritants)
      (%make-match-condition irritants))

    (define (match-violation? x)
      (&match? x))

    ;; ------------------------------------------------------------------
    ;; match-ellipsis? : syntax-or-datum -> boolean
    ;;   True for the ellipsis marker `...` and extended ellipses
    ;;   `(... n)`, `(... min #t)`, `(... min max)`.
    (define (match-ellipsis? x)
      (let ((f (if (syntax? x) (syntax->datum x) x)))
        (cond ((eq? f '...) #t)
              ((and (pair? f) (eq? (car f) '...)) #t)
              (else #f))))

    ;; define-pattern-syntax : id transformer-expr
    ;;   Binds id to a transformer whose value is a procedure.  The
    ;;   matcher recovers it with syntax-local-value at expansion time.

    (define-syntax define-pattern-syntax
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (datum->syntax
           stx
           (list 'define-syntax
                 (cadr form)
                 (list 'lambda (list 'stx)
                       (list (caddr form) 'stx)))))))


    (define (%bitwise-bit-set? bits n)
      (not (zero? (logand bits (ash 1 n)))))

    (define (%bitwise-bit-set bits n)
      (logior bits (ash 1 n)))

    ;; NFA virtual machine runtime.
    ;;
    ;; instruction ::= (type arg0 arg1)
    ;;   'test   arg0 = action procedure, arg1 unused
    ;;   'fork   arg0 = continue pc, arg1 = fork pc
    ;;   'branch arg0 = jump pc, arg1 unused
    ;;   'end    both unused
    ;;
    ;; A register set is a vector; element 0 holds a copy-on-write
    ;; counter, register n lives at element n+1.

    (define (%make-registers size)
      (let ((vec (make-vector (+ size 1) '())))
        (vector-set! vec 0 0)
        vec))

    (define (%registers-cow! regs)
      (vector-set! regs 0 (+ 1 (vector-ref regs 0))))

    (define (%registers-set! regs . idxs+vals)
      (if (eqv? (vector-ref regs 0) 0)
        (begin
          (let loop ((more idxs+vals))
            (unless (null? more)
              (vector-set! regs (+ 1 (car more)) (cadr more))
              (loop (cddr more))))
          regs)
        (let ((new-regs (%make-registers (- (vector-length regs) 1))))
          (let loop ((idx 1))
            (unless (>= idx (vector-length regs))
              (vector-set! new-regs idx (vector-ref regs idx))
              (loop (+ idx 1))))
          (vector-set! regs 0 (- (vector-ref regs 0) 1))
          (apply %registers-set! new-regs idxs+vals))))

    (define (%register-ref regs idx)
      (vector-ref regs (+ idx 1)))

    (define-record-type %instruction
      (%make-instruction type arg0 arg1)
      %instruction?
      (type %instruction-type)
      (arg0 %instruction-arg0)
      (arg1 %instruction-arg1))

    (define-record-type %thread
      (%make-thread pc id registers)
      %thread?
      (pc %thread-pc %thread-pc-set!)
      (id %thread-id)
      (registers %thread-registers %thread-registers-set!))

    (define-record-type %vm-record
      (%make-%vm-record instructions current-threads next-threads
                        current-pcs next-pcs dead-thread)
      %vm-record?
      (instructions %vm-record-instructions)
      (current-threads %vm-record-current-threads %vm-record-current-threads-set!)
      (next-threads %vm-record-next-threads %vm-record-next-threads-set!)
      (current-pcs %vm-record-current-pcs %vm-record-current-pcs-set!)
      (next-pcs %vm-record-next-pcs %vm-record-next-pcs-set!)
      (dead-thread %vm-record-dead-thread %vm-record-dead-thread-set!))

    (define (%vm-make-thread vm pc registers)
      (if (%vm-record-dead-thread vm)
        (let ((thread (%vm-record-dead-thread vm)))
          (%vm-record-dead-thread-set! vm #f)
          (%thread-pc-set! thread pc)
          (%thread-registers-set! thread registers)
          thread)
        (%make-thread pc 0 registers)))

    (define (%vm-kill-thread! vm thread)
      (%vm-record-dead-thread-set! vm thread))

    (define (%vm-add-thread! vm thread)
      (let ((pc (%thread-pc thread)))
        (if (not (%bitwise-bit-set? (%vm-record-next-pcs vm) pc))
          (begin
            (%vm-record-next-pcs-set! vm
                                      (%bitwise-bit-set (%vm-record-next-pcs vm) pc))
            (let ((instruction
                   (vector-ref (%vm-record-instructions vm) pc)))
              (case (%instruction-type instruction)
                ((fork)
                 (%thread-pc-set! thread (%instruction-arg0 instruction))
                 (%registers-cow! (%thread-registers thread))
                 (%vm-add-thread! vm thread)
                 (%vm-add-thread! vm
                                  (%vm-make-thread
                                   vm
                                   (%instruction-arg1 instruction)
                                   (%thread-registers thread))))
                ((branch)
                 (%thread-pc-set! thread (%instruction-arg0 instruction))
                 (%vm-add-thread! vm thread))
                (else
                 (let ((threads (%vm-record-next-threads vm)))
                   (let loop ((idx 0))
                     (if (not (vector-ref threads idx))
                       (vector-set! threads idx thread)
                       (loop (+ idx 1))))))))))))

    (define (%vm-swap-threads! vm)
      (let ((current (%vm-record-current-threads vm)))
        (%vm-record-current-threads-set! vm (%vm-record-next-threads vm))
        (%vm-record-current-pcs-set! vm (%vm-record-next-pcs vm))
        (vector-fill! current #f)
        (%vm-record-next-threads-set! vm current)
        (%vm-record-next-pcs-set! vm 0)))

    (define (%make-vm instructions n-registers n-tests)
      (let ((vm (%make-%vm-record instructions
                                  (make-vector (+ n-tests 1) #f)
                                  (make-vector (+ n-tests 1) #f)
                                  0 0 #f)))
        (%vm-add-thread! vm (%make-thread 0 0 (%make-registers n-registers)))
        (%vm-swap-threads! vm)
        vm))

    (define (%vm-destroy! vm)
      (%vm-record-current-threads-set! vm #f)
      (%vm-record-next-threads-set! vm #f)
      (%vm-record-dead-thread-set! vm #f))

    (define (%vm-alive? vm)
      (not (zero? (%vm-record-current-pcs vm))))

    (define (%vm-each-current-thread vm proc)
      (let ((threads (%vm-record-current-threads vm)))
        (let loop ((idx 0))
          (when (< idx (vector-length threads))
            (let ((thread (vector-ref threads idx)))
              (when thread (proc thread))
              (loop (+ idx 1)))))))

    (define (%vm-finished-thread vm)
      (let ((threads (%vm-record-current-threads vm)))
        (let loop ((idx (- (vector-length threads) 1)))
          (if (< idx 0)
            #f
            (let ((thread (vector-ref threads idx)))
              (if (and thread
                       (eq? 'end
                            (%instruction-type
                             (vector-ref (%vm-record-instructions vm)
                                         (%thread-pc thread)))))
                thread
                (loop (- idx 1))))))))

    (define (%vm-step! vm input)
      (let ((maybe-match #f))
        (let ((instructions (%vm-record-instructions vm)))
      (%vm-each-current-thread vm
        (lambda (thread)
          (let ((instruction
                 (vector-ref instructions (%thread-pc thread))))
                (case (%instruction-type instruction)
                  ((test)
                   (let ((new-regs ((%instruction-arg0 instruction)
                                    input (%thread-registers thread))))
                     (if new-regs
                       (begin
                         (%thread-pc-set! thread (+ (%thread-pc thread) 1))
                         (%thread-registers-set! thread new-regs)
                         (%vm-add-thread! vm thread))
                       (%vm-kill-thread! vm thread))))
                  ((end)
                   (set! maybe-match (%thread-registers thread))
                   (%vm-kill-thread! vm thread)))))))
        (%vm-swap-threads! vm)
        (let ((ft (%vm-finished-thread vm)))
          (if ft (%thread-registers ft) maybe-match))))

    (define (%vm-prune-nonfinal-threads! vm)
      (let ((final-pc (- (vector-length (%vm-record-instructions vm)) 2)))
        (%vm-each-current-thread vm
          (lambda (thread)
            (if (eqv? (%thread-pc thread) final-pc)
              (%vm-add-thread! vm thread))))
        (%vm-swap-threads! vm)))

    ;; ------------------------------------------------------------------
    ;; Pattern expansion to core form.
    ;;
    ;;   (wildcard) (var id) (quote datum)
    ;;   (seq-list pat ...) (seq-cons a d) (seq-vector pat ...)
    ;;   (seq-and pat ...) (seq-or pat ...) (seq-not pat)
    ;;   (seq-pred proc (pat ...)) (seq-proj proc (pat ...))
    ;;   (seq kind name state term ref (seq:one (vars) pat)
    ;;        (seq:many (vars) min max pat) ...)
    ;;     kind = ordered | partial | unordered
    ;;     name = identifier bound to the subject once
    ;;     state = ((var init step) ...)
    ;;     term/ref = termination / iteration expressions (datum)

    (define-syntax match
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((expr (syntax->datum (cadr form)))
                (clauses (map syntax-form (cddr form))))
            (if (null? clauses)
              (error "match: no clauses")
              (let ((code
                     (let loop ((cls clauses))
                       (if (null? (cdr cls))
                         (let* ((pat (caar cls))
                                (body (cdar cls)))
                           (let* ((core (expand-pattern pat))
                                  (vars (collect-vars core))
                                  (binds (map (lambda (v)
                                                (cons v (car (generate-temporaries (list v)))))
                                              vars))
                                  (renamed-body (rename-body binds
                                                             (map syntax->datum body))))
                            (call-with-values
                              (lambda ()
                                (gen* core expr
                                      '(raise (make-match-violation))
                                      (cons 'begin renamed-body)
                                      binds))
                              (lambda (code ignored) code))))
                        (let* ((pat (caar cls))
                               (body (cdar cls)))
                          (let* ((core (expand-pattern pat))
                                 (vars (collect-vars core))
                                 (binds (map (lambda (v)
                                               (cons v (car (generate-temporaries (list v)))))
                                             vars))
                                 (renamed-body (rename-body binds
                                                            (map syntax->datum body))))
                            (call-with-values
                              (lambda ()
                                (gen* core expr
                                      (loop (cdr cls))
                                      (cons 'begin renamed-body)
                                      binds))
                              (lambda (code ignored) code))))))))
                (begin
                  (datum->syntax stx code))))))))

    ;; match-lambda : ((pattern ...) body ...) ... -> procedure
    ;;   Clauses are grouped by argument count and dispatched with
    ;;   case-lambda.
    (define-syntax match-lambda
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((clauses (cdr form)))
            (if (null? clauses)
              (error "match-lambda: no clauses")
              (let* ((groups
                      (let loop ((cls clauses) (acc '()))
                        (if (null? cls)
                          acc
                          (let* ((cl (syntax-form (car cls)))
                                 (pat-datum (syntax->datum (car cl)))
                                 (arity (if (list? pat-datum)
                                          (length pat-datum)
                                          1)))
                            (let ((entry (assv arity acc)))
                              (if entry
                                (begin
                                  (set-cdr! entry (cons (car cls) (cdr entry)))
                                  (loop (cdr cls) acc))
                                (loop (cdr cls)
                                      (cons (cons arity (list (car cls)))
                                            acc)))))))))
                (datum->syntax
                 stx
                 (cons 'case-lambda
                       (map (lambda (g)
                              (let* ((arity (car g))
                                     (args (generate-temporaries
                                            (make-list arity 'a))))
                                (list args
                                      (compile-group arity (cdr g) args))))
                            groups)))))))))

    ;; ------------------------------------------------------------------
    ;; Derived forms.

    ;; ------------------------------------------------------------------
    ;; Derived forms.

    ;; match-values : expr ((pattern ...) body ...) ... -> value
    (define-syntax match-values
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((expr (cadr form))
                (clauses (cddr form)))
            (datum->syntax
             stx
             `(call-with-values
               (lambda () ,expr)
               ,(cons 'match-lambda clauses)))))))

    ;; match-let : ((pat init) ...) body ... -> value
    (define-syntax match-let
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (list 'match-values
                   (cons 'values
                         (map (lambda (b)
                                (syntax->datum (cadr (syntax-form b))))
                              binds))
                   (cons (map (lambda (b) (car (syntax-form b))) binds)
                         body)))))))

    ;; match-let* : sequential version
    (define-syntax match-let*
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (if (null? binds)
               (cons 'let (cons '() (map syntax->datum body)))
               (let* ((first (syntax-form (car binds)))
                      (first-pat (car first))
                      (first-init (syntax->datum (cadr first))))
                 (list 'match-values
                       first-init
                       (list (list first-pat)
                             (cons 'match-let*
                                   (cons (cdr binds) body)))))))))))

    ;; match-let-values : (((pat ...) init) ...) body ... -> value
    (define-syntax match-let-values
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((groups (syntax->datum (cadr form)))
                (body (syntax->datum (cddr form))))
            (let* ((pats-groups (map car groups))
                   (inits (map (lambda (g) (cadr g)) groups))
                   (temps-groups
                    (let loop ((gs pats-groups) (counter 1) (acc '()))
                      (if (null? gs)
                        (reverse acc)
                        (let loop2 ((ps (car gs)) (cnt counter) (acc2 '()))
                          (if (null? ps)
                            (loop (cdr gs) cnt (cons (reverse acc2) acc))
                            (loop2 (cdr ps) (+ cnt 1)
                                   (cons (string->symbol
                                          (string-append
                                           "mlv-tmp-"
                                           (number->string cnt)))
                                         acc2)))))))
                   (clause (cons (apply append pats-groups) body))
                   (match-call
                    (cons (cons 'match-lambda (list clause))
                          (apply append temps-groups))))
              (datum->syntax
               stx
               (let cwv ((tgs temps-groups) (inits inits))
                 (if (null? tgs)
                   match-call
                   (list 'call-with-values
                         (list 'lambda '() (car inits))
                         (list 'lambda (car tgs)
                               (cwv (cdr tgs) (cdr inits))))))))))))

    ;; match-let*-values : sequential version
    (define-syntax match-let*-values
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (if (null? binds)
               (cons 'let (cons '() (map syntax->datum body)))
               (let* ((first (syntax-form (car binds)))
                      (first-pats (car first))
                      (first-init (syntax->datum (cadr first))))
                 (list 'match-let-values
                       (list (list first-pats first-init))
                       (cons 'match-let*-values
                             (cons (cdr binds) body))))))))))

    ;; match-define : (match-define pattern expr) -> void
    (define-syntax match-define
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (datum->syntax
           stx
           (list 'match-define-values (list (cadr form)) (caddr form))))))

    (define-syntax match-define-values
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((pats (syntax->datum (cadr form)))
                (expr (syntax->datum (caddr form))))
            (let* ((cores (map expand-pattern pats))
                   (vars (apply append (map collect-vars cores)))
                   (args (let loop ((i 0) (ps pats) (acc '()))
                           (if (null? ps)
                             (reverse acc)
                             (loop (+ i 1) (cdr ps)
                                   (cons (string->symbol
                                          (string-append
                                           "mdv-arg-"
                                           (number->string i)))
                                         acc)))))
                   (code (compile-pats-gen*
                          pats args
                          (list (cons 'values vars))
                          '(error 'match "no matching pattern"))))
              (datum->syntax
               stx
               ;; The define-values head resolves in THIS library (it is
               ;; imported from (scheme base) here), not at the use site:
               ;; datum->syntax attaches the use-site context, so splice a
               ;; definition-site identifier in for the head only (the rest
               ;; -- vars, case-lambda, the user's expr -- stays use-site).
               (list (datum->syntax (quote-syntax define-values)
                                    'define-values)
                     vars
                     (list (cons 'case-lambda
                                 (list (cons args (list code))))
                           expr))))))))

    ;; match-letrec : ((pat init) ...) body ... -> value
    ;;   Compiled to (let ((v #f) ... (t #f) ...)
    ;;                (set! t init) (set! v (match t (pat v) ...)) ...
    ;;                (let () body ...))
    ;;   which avoids internal define-values (goldfish expander bug 6)
    ;;   while keeping pattern variables visible to later inits.
    (define-syntax match-letrec
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (list 'match-values
                   (cons 'values
                         (map (lambda (b)
                                (syntax->datum (cadr (syntax-form b))))
                              binds))
                   (cons (map (lambda (b) (car (syntax-form b))) binds)
                         body)))))))

    ;; match-letrec* : sequential version; each init sees the pattern
    ;;   variables bound by the patterns to its left.
    (define-syntax match-letrec*
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (if (null? binds)
              (datum->syntax
               stx
               (cons 'let (cons '() (map syntax->datum body))))
              (let* ((first (syntax-form (car binds)))
                     (first-pats (car first))
                     (first-init (syntax->datum (cadr first))))
                (datum->syntax
                 stx
                 (list 'match-letrec
                       (list (list first-pats first-init))
                       (cons 'match-letrec*
                             (cons (cdr binds) body))))))))))

    ;; if-match : ((pat init) ...) conseq alter -> value
    (define-syntax if-match
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (if (< (length form) 4)
            (error "if-match: malformed form")
            (let* ((binds (syntax-form (cadr form)))
                   (groups (map (lambda (b) (syntax-form b)) binds))
                   (pats (map car groups))
                   (pats-datum (map syntax->datum pats))
                   (inits (map (lambda (g) (syntax->datum (cadr g))) groups))
                   (conseq (syntax->datum (caddr form)))
                   (alter (syntax->datum (cadddr form))))
               (datum->syntax
                stx
                (list 'match-values
                      (cons 'values inits)
                      (cons pats-datum (list conseq))
                      (cons (map (lambda (_) '_) pats-datum)
                            (list alter)))))))))))


