;;; bytecode.scm -- L2: bytecode compilation front-end (IR version).
;;;
;;; Compiles the record IR (goldfish/compiler/ir.scm) into a serializable
;;; instruction sequence for the future C++ VM.  The bytecode is plain
;;; data -- symbols, numbers, lists -- so it round-trips through write/read
;;; and can be cached on disk.  The passes above remain the front-end;
;;; only the executor is swapped.
;;;
;;; Bytecode shape (a stack machine):
;;;   (program (code-table code...) (top instr...))
;;;   code = (code <nlocals> <formals> instr...)
;;;   instr = (const v) | (global name) | (ref d i) | (local i)
;;;         | (set-local i) | (set-ref d i) | (store-global name)
;;;         | (closure i) | (call n) | (tail-call n)
;;;         | (if-else L) | (jump L) | (label L) | (return)
;;;         | (values n) | (call-with-values) | (pop)
;;;
;;; Lexical addressing: every frame owns slots 0..nlocals-1 (formals,
;;; then let/letrec bindings and internal defines).  A variable of the
;;; current frame is (local i); a variable of an enclosing frame is
;;; (ref d i) with d the frame distance (1 = the frame that created the
;;; closure); anything else is (global name).  The VM's closure captures
;;; the enclosing frame's slots, so ref/set-ref resolve through the
;;; captured chain.
;;;
;;; Conventions:
;;; - An application in tail position compiles to (tail-call n), a
;;;   jump that never returns; otherwise (call n) pushes the result.
;;; - call-with-values with a producer that is a lambda whose tail is
;;;   a values form is inlined into a direct consumer call with a
;;;   statically known arity; any other shape falls back to the
;;;   (call-with-values) instruction.

(define-library (goldfish compiler bytecode)
  (import (scheme base)
          (goldfish compiler ir))
  (export to-bytecode
    valid-bytecode?
    *bytecode-version*)
  (begin

    (define *bytecode-version* 2)

    ;; fold-left : (a b -> a) a (list b) -> a
    ;; R7RS (scheme base) has no fold-left; implement the left fold here.
    (define (fold-left f acc ls)
      (if (null? ls)
        acc
        (fold-left f (f acc (car ls)) (cdr ls))))

    ;; lambda-formals->list : formals -> (list symbol)
    (define (lambda-formals->list formals)
      (if (symbol? formals)
        (list formals)
        (let loop ((f formals) (acc '()))
          (cond ((null? f) (reverse acc))
                ((pair? f) (loop (cdr f) (cons (car f) acc)))
                (else (reverse (cons f acc)))))))

    ;; make-slot-env helpers: an association list of (name . slot).
    (define (slot-env-extend env name slot)
      (cons (cons name slot) env))

    ;; make-code-collector : -> (values (code -> index) (-> (list code)))
    (define (make-code-collector)
      (let ((codes '())
            (n 0))
        (values (lambda (code)
                  (let ((i n))
                    (set! codes (cons code codes))
                    (set! n (+ n 1))
                    i))
                (lambda () (reverse codes)))))

    ;; resolve-var : frame-envs name -> (list d slot) or #f
    ;; frame-envs is a list of per-frame alists, the current frame first.
    ;; Returns (d slot) with d the frame distance, or #f for a global.
    (define (resolve-var envs name)
      (let loop ((es envs) (d 0))
        (if (null? es)
          #f
          (let ((cell (assq name (car es))))
            (if cell
              (list d (cdr cell))
              (loop (cdr es) (+ d 1)))))))

    ;; compile-lambda : formals body frame-envs code-add -> index
    ;; Compile a lambda literal into a code object, register it with
    ;; code-add, and return its index.  Each lambda has an independent
    ;; frame; nested lambdas register their code first (post-order).
    (define (compile-lambda formals body outer-envs add-code)
      (let ((instr '())
            (slot-n 0)
            (label-n 0))
        (define (emit i) (set! instr (cons i instr)))
        (define (flush) (reverse instr))
        (define (next-label) (let ((l label-n)) (set! label-n (+ label-n 1)) l))
        (define (next-slot) (let ((s slot-n)) (set! slot-n (+ slot-n 1)) s))
        (let* ((names (lambda-formals->list formals))
               (alist (fold-left (lambda (e n) (slot-env-extend e n (next-slot)))
                                 '() names))
               (envs (cons alist outer-envs)))
          (compile-body body #t envs emit next-label next-slot add-code)
          (add-code (list 'code slot-n formals (flush))))))

    ;; compile-body : (list ir) bool frame-envs emit next-label next-slot
    ;;                code-add -> void
    ;; Non-final expressions are evaluated and popped; internal defines
    ;; become frame slots bound with letrec* semantics.  tail? is whether
    ;; the final expression is in tail position (emit (return) / tail-call):
    ;; a let in non-tail position must leave its value on the stack instead.
    (define (compile-body body tail? envs emit next-label next-slot add-code)
      (let loop ((bs body) (envs envs))
        (cond
          ((null? bs)
           (error "to-bytecode: empty lambda body"))
          ((and (define? (car bs)))
           (let* ((d (car bs))
                  (name (define-name d))
                  (slot (next-slot))
                  (new-alist (slot-env-extend (car envs) name slot))
                  (new-envs (cons new-alist (cdr envs))))
             (compile-expr (define-value d) new-envs emit next-label next-slot add-code)
             (emit (list 'set-local slot))
             (loop (cdr bs) new-envs)))
          ((null? (cdr bs))
           (if tail?
             (compile-tail (car bs) envs emit next-label next-slot add-code)
             (compile-expr (car bs) envs emit next-label next-slot add-code)))
          (else
           (compile-expr (car bs) envs emit next-label next-slot add-code)
           (emit '(pop))
           (loop (cdr bs) envs)))))

    ;; compile-tail : ir frame-envs emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is the result of the enclosing
    ;; lambda: applications become tail calls, the value is left on the
    ;; stack for a trailing (return).
    (define (compile-tail s envs emit next-label next-slot add-code)
      (cond
        [(lexical-ref? s)
         (if (= (lexical-ref-depth s) 0)
           (emit (list 'local (lexical-ref-index s)))
           (emit (list 'ref (lexical-ref-depth s) (lexical-ref-index s))))
         (emit '(return))]
        [(symbol? s)
         (compile-expr s envs emit next-label next-slot add-code)
         (emit '(return))]
        [(or (const? s) (void? s))
         (emit (list 'const (if (const? s) (const-value s) (list 'quote 'void))))
         (emit '(return))]
        [(lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body s)
                                              envs add-code)))
         (emit '(return))]
        [(if? s)
         (let ((els (if-else s)))
           (compile-expr (if-test s) envs emit next-label next-slot add-code)
           (let ((L (next-label)))
             (emit (list 'if-else L))
             (compile-tail (if-then s) envs emit next-label next-slot add-code)
             (emit (list 'label L))
             (if els
               (compile-tail els envs emit next-label next-slot add-code)
               (begin
                 (emit '(const #f))
                 (emit '(return))))))]
        [(begin? s)
         (if (null? (begin-body s))
           (begin
             (emit '(const #f))
             (emit '(return)))
           (compile-body (begin-body s) #t envs emit next-label next-slot add-code))]
        [(let? s)
         (compile-let 'let (let-bindings s) (let-body s) #t envs emit
                      next-label next-slot add-code)]
        [(letrec? s)
         (compile-let 'letrec (letrec-bindings s) (letrec-body s) #t envs emit
                      next-label next-slot add-code)]
        [(set!? s)
         (compile-expr (set!-expr s) envs emit next-label next-slot add-code)
         (let ((t (set!-target s))
               (r (if (lexical-ref? (set!-target s)) #f (resolve-var envs (set!-target s)))))
           (cond
             ((lexical-ref? t)
              (if (= (lexical-ref-depth t) 0)
                (emit (list 'set-local (lexical-ref-index t)))
                (emit (list 'set-ref (lexical-ref-depth t) (lexical-ref-index t)))))
             ((and r (= (car r) 0)) (emit (list 'set-local (cadr r)))
              (r (emit (list 'set-ref (car r) (cadr r)))))
             (else (emit (list 'store-global t)))))
         (emit '(return))]
        [(call-with-values? s)
         (compile-call-with-values (cwv-producer s) (cwv-consumer s) envs emit
                                   next-label next-slot add-code #t)]
        [(values? s)
         (for-each (lambda (e)
                     (compile-expr e envs emit next-label next-slot add-code))
                   (values-args s))
         (emit (list 'values (length (values-args s))))
         (emit '(return))]
        [(call? s)
         (compile-expr (call-proc s) envs emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a envs emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'tail-call (length (call-args s))))]
        [(not (pair? s))
         (emit (list 'const s))
         (emit '(return))]
        [else
         (error "to-bytecode: unknown expression" s)]))

    ;; compile-expr : ir frame-envs emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is pushed onto the stack.
    (define (compile-expr s envs emit next-label next-slot add-code)
      (cond
        ((primitive-ref? s)
         (emit (list 'global (primitive-ref-name s))))
        ((lexical-ref? s)
         (if (= (lexical-ref-depth s) 0)
           (emit (list 'local (lexical-ref-index s)))
           (emit (list 'ref (lexical-ref-depth s) (lexical-ref-index s)))))
        ((symbol? s)
         (let ((r (resolve-var envs s)))
           (cond
             ((and r (= (car r) 0)) (emit (list 'local (cadr r))))
             (r (emit (list 'ref (car r) (cadr r))))
             (else (emit (list 'global s))))))
        ((or (const? s) (void? s))
         (emit (list 'const (if (const? s) (const-value s) (list 'quote 'void)))))
        ((lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body s)
                                              envs add-code))))
        ((if? s)
         (let ((else (if-else s)))
           (compile-expr (if-test s) envs emit next-label next-slot add-code)
           (let ((L1 (next-label)) (L2 (next-label)))
             (emit (list 'if-else L1))
             (compile-expr (if-then s) envs emit next-label next-slot add-code)
             (emit (list 'jump L2))
             (emit (list 'label L1))
             (if else
               (compile-expr else envs emit next-label next-slot add-code)
               (emit '(const #f)))
             (emit (list 'label L2)))))
        ((begin? s)
         (if (null? (begin-body s))
           (emit '(const #f))
           (let loop ((es (begin-body s)))
             (if (null? (cdr es))
               (compile-expr (car es) envs emit next-label next-slot add-code
                 (emit '(pop))
                 (loop (cdr es)))))))
        ((let? s)
         (compile-let 'let (let-bindings s) (let-body s) #f envs emit
                      next-label next-slot add-code))
        ((letrec? s)
         (compile-let 'letrec (letrec-bindings s) (letrec-body s) #f envs emit
                      next-label next-slot add-code))
        ((set!? s)
         (compile-expr (set!-expr s) envs emit next-label next-slot add-code)
         (let ((t (set!-target s))
               (r (if (lexical-ref? (set!-target s)) #f (resolve-var envs (set!-target s)))))
           (cond
             ((lexical-ref? t)
              (if (= (lexical-ref-depth t) 0)
                (emit (list 'set-local (lexical-ref-index t)))
                (emit (list 'set-ref (lexical-ref-depth t) (lexical-ref-index t)))))
             ((and r (= (car r) 0)) (emit (list 'set-local (cadr r))))
             (r (emit (list 'set-ref (car r) (cadr r))))
             (else (emit (list 'store-global t))))))
        ((call-with-values? s)
         (compile-call-with-values (cwv-producer s) (cwv-consumer s) envs emit
                                   next-label next-slot add-code #f))
        ((values? s)
         (for-each (lambda (e)
                     (compile-expr e envs emit next-label next-slot add-code))
                   (values-args s))
         (emit (list 'values (length (values-args s)))))
        ((call? s)
         (compile-expr (call-proc s) envs emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a envs emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'call (length (call-args s)))))
        ((not (pair? s))
         (emit (list 'const s)))
        (else
         (error "to-bytecode: unknown expression" s

           (error "to-bytecode: unknown expression" s)))))

    ;; compile-let : head bindings body tail? frame-envs emit next-label next-slot code-add
    (define (compile-let head bindings body tail? envs emit next-label next-slot add-code)
      (if (eq? head 'let)
        ;; let: inits evaluated in the old env (parallel bindings)
        (let* ((new-alist (fold-left (lambda (e b)
                                       (slot-env-extend e (car b) (next-slot)))
                                     (car envs) bindings))
               (new-envs (cons new-alist (cdr envs))))
          (for-each (lambda (b)
                      (compile-expr (cadr b) envs emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (assq (car b) new-alist)))))
                    bindings)
          (compile-body body tail? new-envs emit next-label next-slot add-code))
        ;; letrec/letrec*: slots allocated first, inits in the new env
        (let* ((new-alist (fold-left (lambda (e b)
                                       (slot-env-extend e (car b) (next-slot)))
                                     (car envs) bindings))
               (new-envs (cons new-alist (cdr envs))))
          (for-each (lambda (b)
                      (compile-expr (cadr b) new-envs emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (assq (car b) new-alist)))))
                    bindings)
          (compile-body body tail? new-envs emit next-label next-slot add-code))))

    ;; static-producer-values : ir -> (values (list ir) (list ir)) or #f
    ;; Recognize a producer (lambda () prelude... (values v...)) and
    ;; return the values arguments and the prelude expressions.
    (define (static-producer-values p)
      (and (lambda? p)
           (null? (lambda-formals->list (lambda-formals p)))
           (pair? (lambda-body p))
           (let* ((body (lambda-body p))
                  (rev (reverse body))
                  (last (car rev)))
             (and (values? last)
                  (list (values-args last) (reverse (cdr rev)))))))

    ;; compile-call-with-values : producer consumer frame-envs emit next-label
    ;;                            next-slot code-add tail? -> void
    ;; Inline a statically known producer (a lambda with a tail values);
    ;; otherwise emit the general (call-with-values) instruction.
    (define (compile-call-with-values p c envs emit next-label next-slot add-code tail?)
      (let ((sv (static-producer-values p)))
        (if sv
          (let ((vals (car sv)) (prelude (cadr sv)))
            (for-each (lambda (e)
                        (compile-expr e envs emit next-label next-slot add-code)
                        (emit '(pop)))
                      prelude)
            ;; consumer is the function: push it first (the call stack
            ;; convention is [f a1 ... an]), then the values as args.
            (if (lambda? c)
              (emit (list 'closure (compile-lambda (lambda-formals c) (lambda-body c)
                                                   envs add-code)))
              (compile-expr c envs emit next-label next-slot add-code))
            (for-each (lambda (e)
                        (compile-expr e envs emit next-label next-slot add-code))
                      vals)
            (if tail?
              (emit (list 'tail-call (length vals)))
              (emit (list 'call (length vals)))))
          (begin
            (compile-expr p envs emit next-label next-slot add-code)
            (compile-expr c envs emit next-label next-slot add-code)
            (emit '(call-with-values))
            (if tail? (emit '(return)))))))

    ;; to-bytecode : (list ir) -> program
    ;; Compile a list of top-level IR defs.  Top-level defines store
    ;; their value into the global binding after evaluation; lambda-valued
    ;; ones go through the code table.  The top level has no lexical
    ;; frame, so every top-level variable reference is a (global ...).
    (define (to-bytecode defs)
      (let-values (((add-code get-codes) (make-code-collector)))
        (let ((instr '())
              (slot-n 0)
              (label-n 0))
          (define (emit i) (set! instr (cons i instr)))
          (define (flush) (reverse instr))
          (define (next-label) (let ((l label-n)) (set! label-n (+ label-n 1)) l))
          (define (next-slot) (let ((s slot-n)) (set! slot-n (+ slot-n 1)) s))
          (for-each
            (lambda (d)
              (if (define? d)
                (begin
                  ;; A define value that is itself a let/letrec (not a
                  ;; lambda) compiles against a non-empty frame env for its
                  ;; slot allocation.  A lambda value must NOT see the empty
                  ;; top-level alist: it would shift nested-lambda capture
                  ;; depths by one (compiled refs vs the runtime chain).
                  (compile-expr (define-value d)
                                (if (lambda? (define-value d)) '() '(()))
                                emit next-label next-slot add-code)
                  (emit (list 'store-global (define-name d))))
                ;; A top-level expression (a library registration side
                ;; effect, say) compiles against a non-empty frame: lambda
                ;; bodies never touch the top frame env, but an expression
                ;; containing let/letrec takes (car envs).
                (compile-expr d '(()) emit next-label next-slot add-code)))
            defs)
          (list 'program
                (cons 'code-table (get-codes))
                (cons 'top (cons slot-n (flush)))))))

    ;; ------------------------------------------------------------------
    ;; Bytecode structural validation.
    ;;
    ;; valid-bytecode? : program -> boolean
    ;; Check that every instruction is well-formed, local/closure
    ;; indices are in range, and every jump target names an existing
    ;; label.  Used by the test suite and as a sanity check before a
    ;; program is handed to the VM backend.

    (define (collect-instr-labels instr-lists)
      (let loop ((ls instr-lists) (acc '()))
        (if (null? ls)
          acc
          (loop (cdr ls)
                (let loop2 ((is (car ls)) (acc acc))
                  (if (null? is)
                    acc
                    (loop2 (cdr is)
                           (if (and (pair? (car is)) (eq? (caar is) 'label))
                             (cons (cadar is) acc)
                             acc))))))))

    (define (valid-instr? i labels ncode nlocals)
      (case (car i)
        [(const) (>= (length i) 2)]
        [(global store-global)
         (and (>= (length i) 2) (symbol? (cadr i)))]
        [(local set-local)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0)
              (or (not nlocals) (< (cadr i) nlocals)))]
        [(ref set-ref)
         (and (>= (length i) 3) (integer? (cadr i)) (>= (cadr i) 1)
              (integer? (caddr i)) (>= (caddr i) 0))]
        [(closure)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0)
              (< (cadr i) ncode))]
        [(call tail-call values)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0))]
        [(if-else jump)
         (and (>= (length i) 2) (member (cadr i) labels))]
        [(label) (and (>= (length i) 2) (member (cadr i) labels))]
        [(return pop call-with-values) (null? (cdr i))]
        [else #f]))

    (define (valid-bytecode? bc)
      (and (pair? bc)
           (eq? (car bc) 'program)
           (pair? (cadr bc))
           (eq? (caadr bc) 'code-table)
           (pair? (caddr bc))
           (eq? (caaddr bc) 'top)
           (let* ((codes (cdadr bc))
                  (top (cdaddr bc))
                  (ncode (length codes))
                  (labels (collect-instr-labels
                           (append (map (lambda (c) (cadddr c)) codes)
                                   (list (cdr top))))))
             (and (every (lambda (code)
                           (and (pair? code)
                                (eq? (car code) 'code)
                                (integer? (cadr code))
                                (>= (cadr code) 0)
                                (every (lambda (i)
                                         (valid-instr? i labels ncode (cadr code)))
                                       (cadddr code))))
                         codes)
                  (every (lambda (i) (valid-instr? i labels ncode #f)) (cdr top))))))))
