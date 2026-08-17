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
;;;   code = (code <nlocals> instr...)
;;;   instr = (const v) | (ref name) | (local i) | (set-local i)
;;;         | (store-global name) | (closure i) | (call n)
;;;         | (tail-call n) | (if-else L) | (jump L) | (label L)
;;;         | (return) | (values n) | (call-with-values) | (pop)
;;;
;;; Conventions:
;;; - An application in tail position compiles to (tail-call n), a
;;;   jump that never returns; otherwise (call n) pushes the result.
;;; - A variable with a frame slot compiles to (local i); otherwise it
;;;   is referenced by name (ref name) and captured by the VM's
;;;   closure environment.  Slots are per-frame and start at 0 for the
;;;   lambda formals, then let/letrec bindings and internal defines.
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

    (define *bytecode-version* 1)

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
    (define (slot-env-lookup env name)
      (assq name env))
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

    ;; compile-lambda : formals body code-add -> index
    ;; Compile a lambda literal into a code object, register it with
    ;; code-add, and return its index.  Each lambda has an independent
    ;; frame: formals occupy slots starting at 0, nested lambdas register
    ;; their code first (post-order) so closure references never dangle.
    (define (compile-lambda formals body add-code)
      (let ((instr '())
            (slot-n 0)
            (label-n 0))
        (define (emit i) (set! instr (cons i instr)))
        (define (flush) (reverse instr))
        (define (next-label) (let ((l label-n)) (set! label-n (+ label-n 1)) l))
        (define (next-slot) (let ((s slot-n)) (set! slot-n (+ slot-n 1)) s))
        (let* ((names (lambda-formals->list formals))
               (env1 (fold-left (lambda (e n) (slot-env-extend e n (next-slot)))
                                '() names)))
          (compile-body body env1 emit next-label next-slot add-code)
          (add-code (list 'code slot-n (flush))))))

    ;; compile-body : (list ir) env emit next-label next-slot code-add -> void
    ;; Non-final expressions are evaluated and popped; internal defines
    ;; become frame slots bound with letrec* semantics.
    (define (compile-body body env emit next-label next-slot add-code)
      (let loop ((bs body) (env env))
        (cond
          ((null? bs)
           (error "to-bytecode: empty lambda body"))
          ((and (define? (car bs)))
           (let* ((d (car bs))
                  (name (define-name d))
                  (slot (next-slot))
                  (env1 (slot-env-extend env name slot)))
             (compile-expr (define-value d) env1 emit next-label next-slot add-code)
             (emit (list 'set-local slot))
             (loop (cdr bs) env1)))
          ((null? (cdr bs))
           (compile-tail (car bs) env emit next-label next-slot add-code))
          (else
           (compile-expr (car bs) env emit next-label next-slot add-code)
           (emit '(pop))
           (loop (cdr bs) env)))))

    ;; compile-tail : ir env emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is the result of the enclosing
    ;; lambda: applications become tail calls, the value is left on the
    ;; stack for a trailing (return).
    (define (compile-tail s env emit next-label next-slot add-code)
      (cond
        ((symbol? s)
         (compile-expr s env emit next-label next-slot add-code)
         (emit '(return)))
        ((or (const? s) (void? s))
         (emit (list 'const (if (const? s) (const-value s) (list 'quote 'void))))
         (emit '(return)))
        ((lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body s) add-code)))
         (emit '(return)))
        ((if? s)
         (let ((else (if-else s)))
           (compile-expr (if-test s) env emit next-label next-slot add-code)
           (let ((L (next-label)))
             (emit (list 'if-else L))
             (compile-tail (if-then s) env emit next-label next-slot add-code)
             (emit (list 'label L))
             (if else
               (compile-tail else env emit next-label next-slot add-code)
               (begin
                 (emit '(const #f))
                 (emit '(return)))))))
        ((begin? s)
         (if (null? (begin-body s))
           (begin
             (emit '(const #f))
             (emit '(return)))
           (compile-body (begin-body s) env emit next-label next-slot add-code)))
        ((let? s)
         (compile-let 'let (let-bindings s) (let-body s) env emit
                      next-label next-slot add-code))
        ((letrec? s)
         (compile-let 'letrec (letrec-bindings s) (letrec-body s) env emit
                      next-label next-slot add-code))
        ((set!? s)
         (compile-expr (set!-expr s) env emit next-label next-slot add-code)
         (let ((cell (slot-env-lookup env (set!-target s))))
           (if cell
             (emit (list 'set-local (cdr cell)))
             (emit (list 'store-global (set!-target s)))))
         (emit '(return)))
        ((call-with-values? s)
         (compile-call-with-values (cwv-producer s) (cwv-consumer s) env emit
                                   next-label next-slot add-code #t))
        ((values? s)
         (for-each (lambda (e)
                     (compile-expr e env emit next-label next-slot add-code))
                   (values-args s))
         (emit (list 'values (length (values-args s))))
         (emit '(return)))
        ((call? s)
         (compile-expr (call-proc s) env emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a env emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'tail-call (length (call-args s)))))
        ((not (pair? s))
         (emit (list 'const s))
         (emit '(return)))
        (else
         (error "to-bytecode: unknown expression" s))))

    ;; compile-expr : ir env emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is pushed onto the stack.
    (define (compile-expr s env emit next-label next-slot add-code)
      (cond
        ((symbol? s)
         (let ((cell (slot-env-lookup env s)))
           (if cell
             (emit (list 'local (cdr cell)))
             (emit (list 'ref s)))))
        ((or (const? s) (void? s))
         (emit (list 'const (if (const? s) (const-value s) (list 'quote 'void)))))
        ((lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body s) add-code))))
        ((if? s)
         (let ((else (if-else s)))
           (compile-expr (if-test s) env emit next-label next-slot add-code)
           (let ((L1 (next-label)) (L2 (next-label)))
             (emit (list 'if-else L1))
             (compile-expr (if-then s) env emit next-label next-slot add-code)
             (emit (list 'jump L2))
             (emit (list 'label L1))
             (if else
               (compile-expr else env emit next-label next-slot add-code)
               (emit '(const #f)))
             (emit (list 'label L2)))))
        ((begin? s)
         (if (null? (begin-body s))
           (emit '(const #f))
           (let loop ((es (begin-body s)))
             (if (null? (cdr es))
               (compile-expr (car es) env emit next-label next-slot add-code)
               (begin
                 (compile-expr (car es) env emit next-label next-slot add-code)
                 (emit '(pop))
                 (loop (cdr es)))))))
        ((let? s)
         (compile-let 'let (let-bindings s) (let-body s) env emit
                      next-label next-slot add-code))
        ((letrec? s)
         (compile-let 'letrec (letrec-bindings s) (letrec-body s) env emit
                      next-label next-slot add-code))
        ((set!? s)
         (compile-expr (set!-expr s) env emit next-label next-slot add-code)
         (let ((cell (slot-env-lookup env (set!-target s))))
           (if cell
             (emit (list 'set-local (cdr cell)))
             (emit (list 'store-global (set!-target s))))))
        ((call-with-values? s)
         (compile-call-with-values (cwv-producer s) (cwv-consumer s) env emit
                                   next-label next-slot add-code #f))
        ((values? s)
         (for-each (lambda (e)
                     (compile-expr e env emit next-label next-slot add-code))
                   (values-args s))
         (emit (list 'values (length (values-args s)))))
        ((call? s)
         (compile-expr (call-proc s) env emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a env emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'call (length (call-args s)))))
        ((not (pair? s))
         (emit (list 'const s)))
        (else
         (error "to-bytecode: unknown expression" s))))

    ;; compile-let : head bindings body env emit next-label next-slot code-add
    (define (compile-let head bindings body env emit next-label next-slot add-code)
      (if (eq? head 'let)
        ;; let: inits evaluated in the old env (parallel bindings)
        (let* ((new-env (fold-left (lambda (e b)
                                     (slot-env-extend e (car b) (next-slot)))
                                   env bindings)))
          (for-each (lambda (b)
                      (compile-expr (cadr b) env emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (slot-env-lookup new-env (car b))))))
                    bindings)
          (compile-body body new-env emit next-label next-slot add-code))
        ;; letrec/letrec*: slots allocated first, inits in the new env
        (let ((new-env (fold-left (lambda (e b)
                                    (slot-env-extend e (car b) (next-slot)))
                                  env bindings)))
          (for-each (lambda (b)
                      (compile-expr (cadr b) new-env emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (slot-env-lookup new-env (car b))))))
                    bindings)
          (compile-body body new-env emit next-label next-slot add-code))))

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

    ;; compile-call-with-values : producer consumer env emit next-label
    ;;                            next-slot code-add tail? -> void
    ;; Inline a statically known producer (a lambda with a tail values);
    ;; otherwise emit the general (call-with-values) instruction.
    (define (compile-call-with-values p c env emit next-label next-slot add-code tail?)
      (let ((sv (static-producer-values p)))
        (if sv
          (let ((vals (car sv)) (prelude (cadr sv)))
            (for-each (lambda (e)
                        (compile-expr e env emit next-label next-slot add-code)
                        (emit '(pop)))
                      prelude)
            (for-each (lambda (e)
                        (compile-expr e env emit next-label next-slot add-code))
                      vals)
            (if (lambda? c)
              (emit (list 'closure (compile-lambda (lambda-formals c) (lambda-body c) add-code)))
              (compile-expr c env emit next-label next-slot add-code))
            (if tail?
              (emit (list 'tail-call (length vals)))
              (emit (list 'call (length vals)))))
          (begin
            (compile-expr p env emit next-label next-slot add-code)
            (compile-expr c env emit next-label next-slot add-code)
            (emit '(call-with-values))
            (if tail? (emit '(return)))))))

    ;; to-bytecode : (list ir) -> program
    ;; Compile a list of top-level IR defs.  Top-level defines store
    ;; their value into the global binding after evaluation; lambda-valued
    ;; ones go through the code table.
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
                  (compile-expr (define-value d) '() emit next-label next-slot add-code)
                  (emit (list 'store-global (define-name d))))
                (compile-expr d '() emit next-label next-slot add-code)))
            defs)
          (list 'program
                (cons 'code-table (get-codes))
                (cons 'top (flush))))))

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
        ((const) (>= (length i) 2))
        ((ref store-global)
         (and (>= (length i) 2) (symbol? (cadr i))))
        ((local set-local)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0)
              (or (not nlocals) (< (cadr i) nlocals))))
        ((closure)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0)
              (< (cadr i) ncode)))
        ((call tail-call values)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0)))
        ((if-else jump)
         (and (>= (length i) 2) (member (cadr i) labels)))
        ((label) (and (>= (length i) 2) (member (cadr i) labels)))
        ((return pop call-with-values) (null? (cdr i)))
        (else #f)))

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
                           (append (map (lambda (c) (caddr c)) codes)
                                   (list top)))))
             (and (every (lambda (code)
                           (and (pair? code)
                                (eq? (car code) 'code)
                                (integer? (cadr code))
                                (>= (cadr code) 0)
                                (every (lambda (i)
                                         (valid-instr? i labels ncode (cadr code)))
                                       (caddr code))))
                         codes)
                  (every (lambda (i) (valid-instr? i labels ncode #f)) top)))))

    )) ;begin
