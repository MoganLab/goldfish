;;; bytecode.scm -- L2: bytecode compilation front-end (IR version).
;;;
;;; Compiles the record IR (goldfish/core/ir.scm, Guile-aligned tree-il)
;;; into a serializable instruction sequence for the future C++ VM.  The
;;; bytecode is plain data -- symbols, numbers, lists -- so it round-trips
;;; through write/read and can be cached on disk.  The passes above remain
;;; the front-end; only the executor is swapped.
;;;
;;; Bytecode shape (a stack machine):
;;;   (program (code-table code...) (top instr...))
;;;   code = (code <nlocals> <formals> instr...)
;;;   instr = (const v) | (global name) | (ref d i) | (local i)
;;;         | (set-local i) | (set-ref d i) | (store-global name)
;;;         | (closure i) | (call n) | (tail-call n)
;;;         | (if-else L) | (jump L) | (label L) | (return) | (pop)
;;;
;;; Lexical addressing: every frame owns slots 0..nlocals-1 (formals,
;;; then let/letrec bindings and internal defines).  A variable of the
;;; current frame is (local i); a variable of an enclosing frame is
;;; (ref d i) with d the frame distance (1 = the frame that created the
;;; closure); anything else is (global name).  The VM's closure captures
;;; the enclosing frame's slots, so ref/set-ref resolve through the
;;; captured chain.
;;;
;;; IR shape notes: <lambda> carries a single body (a <seq> tree / a
;;; <letrec>); <begin> is a binary <seq>; <if> is <conditional>; <set!>
;;; is typed (<lexical-set> / <toplevel-set>).

(define-library (goldfish compiler bytecode)
  (import (scheme base)
          (goldfish core ir))
  (export to-bytecode
    encode-bytecode
    valid-bytecode?
    *bytecode-version*)
  (begin

    (define *bytecode-version* 2)

    ;; Opcode numbers must match the Op enum in src/goldfish_vm.cpp.
    ;; Pre-release the numbering is unstable: renumber freely, both
    ;; sides together; it freezes into an ABI at the first release.
    ;; (label L) has no opcode -- encoding resolves labels to
    ;; instruction indices and drops them.
    (define vm-opcodes
      '((const . 0) (global . 1) (ref . 2) (local . 3) (set-local . 4)
        (set-ref . 5) (store-global . 6) (closure . 7) (call . 8)
        (tail-call . 9) (if-else . 10) (jump . 11) (return . 12)
        (pop . 13)))

    ;; encode-instrs : (list instr) -> vector
    ;; Four slots per instruction: opcode, payload, i0, i1.  The payload
    ;; carries the const value or the global name (#f when unused); i0/i1
    ;; carry arity, slot or resolved jump target (0 when unused).
    (define (encode-instrs instrs)
      (let ((label->idx
             (let walk ((is instrs) (i 0) (acc '()))
               (cond ((null? is) acc)
                     ((eq? (caar is) 'label)
                      (walk (cdr is) i (cons (cons (cadar is) i) acc)))
                     (else (walk (cdr is) (+ i 1) acc))))))
        (let loop ((is instrs) (acc '()))
          (cond ((null? is)
                 (list->vector (reverse acc)))
                ((eq? (caar is) 'label) (loop (cdr is) acc))
                (else
                 (let* ((instr (car is))
                        (op
                         (cond ((assq (car instr) vm-opcodes) => cdr)
                               (else
                                (error "encode-instrs: unknown instruction"
                                       (car instr)))))
                        (slots
                         (case (car instr)
                           ((const global store-global)
                            (list op (cadr instr) 0 0))
                           ((ref set-ref)
                            (list op #f (cadr instr) (caddr instr)))
                           ((if-else jump)
                            (list op #f
                                  (cond ((assq (cadr instr) label->idx) => cdr)
                                        (else
                                         (error "encode-instrs: unknown label"
                                                (cadr instr))))
                                  0))
                           ((local set-local closure call tail-call)
                            (list op #f (cadr instr) 0))
                           (else (list op #f 0 0)))))
                   (loop (cdr is)
                         (cons (cadddr slots)
                               (cons (caddr slots)
                                     (cons (cadr slots)
                                           (cons (car slots) acc)))))))))))

    ;; encode-bytecode : program -> program
    ;; Same program shape, each instruction list replaced by the flat
    ;; positional vector vm-load executes.  The symbolic form stays the
    ;; serializable/cached representation.
    (define (encode-bytecode prog)
      (list 'program
            (cons 'code-table
                  (map (lambda (code)
                         (list 'code (cadr code) (caddr code)
                               (encode-instrs (cadddr code))))
                       (cdr (cadr prog))))
            (list 'top (cadr (caddr prog))
                  (encode-instrs (cddr (caddr prog))))))

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

    ;; lambda-req : ir -> (list symbol) or #f
    ;; The required formal names of a lambda (via its lambda-case), or #f
    ;; if the body is not a lambda-case (degenerate).
    (define (lambda-req lam)
      (let ((b (lambda-body lam)))
        (if (lambda-case? b) (lambda-case-req b) #f)))

    ;; lambda-formals : ir -> formals
    ;; Reconstruct a formals list from a lambda's lambda-case arity.
    (define (lambda-formals lam)
      (let ((b (lambda-body lam)))
        (if (lambda-case? b)
          (let ((req (lambda-case-req b))
                (opt (lambda-case-opt b))
                (rest (lambda-case-rest b)))
            (cond
              ((and (null? opt) rest) (append req rest))
              ((and (null? opt) (not rest)) req)
              (else (append req opt (if rest (list rest) '())))))
          '())))

    ;; lambda-body-expr : ir -> ir
    ;; The body EXPRESSION of a lambda (unwrapping its lambda-case).  The
    ;; IR stores (make-lambda src meta body) where body is a <lambda-case>;
    ;; the real expression is its lambda-case-body.
    (define (lambda-body-expr lam)
      (let ((b (lambda-body lam)))
        (if (lambda-case? b) (lambda-case-body b) b)))

    ;; seq->list : ir -> (list ir)
    ;; Flatten a seq tree into a list of expressions (head first).
    (define (seq->list s)
      (let collect ((s s) (acc '()))
        (cond ((void? s) (reverse acc))
              ((seq? s) (collect (seq-tail s) (cons (seq-head s) acc)))
              (else (reverse (cons s acc))))))

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

    ;; compile-body : ir bool frame-envs emit next-label next-slot code-add
    ;;                -> void
    ;; Compile a lambda body (a single expression, possibly a seq tree).
    ;; Non-final expressions are evaluated and popped; the final
    ;; expression is compiled in tail position.  tail? is whether the
    ;; final expression is in tail position (emit (return) / tail-call).
    (define (compile-body body tail? envs emit next-label next-slot add-code)
      (if (void? body)
        (begin (emit '(const #f))
               (emit '(return)))
        (let ((es (seq->list body)))          ;; Internal (define name value) forms at the head of a lambda body
          ;; lower to letrec slot bindings (the expander normally does this
          ;; before to-bytecode; core->ir leaves them raw).
          (let split ((es es) (defs '()))
            (if (and (pair? es) (toplevel-define? (car es)))
              (split (cdr es) (cons (car es) defs))
              (if (null? defs)
                (compile-body-seq es tail? envs emit next-label next-slot add-code)
                (let* ((names (map toplevel-define-name (reverse defs)))
                       (vals (map toplevel-define-exp (reverse defs)))
                       (new-alist (fold-left (lambda (e n)
                                               (slot-env-extend e n (next-slot)))
                                             (car envs) names))
                       (new-envs (cons new-alist (cdr envs))))
                  (for-each (lambda (n v)
                              (compile-expr v new-envs emit next-label next-slot add-code)
                              (emit (list 'set-local (cdr (assq n new-alist))))
                              (emit '(pop)))
                            names vals)
                  (compile-body-seq es tail? new-envs emit next-label next-slot add-code))))))))

    ;; compile-body-seq : (list ir) bool frame-envs emit next-label next-slot code-add -> void
    ;; Compile a flattened body expression list (already free of internal
    ;; defines): the last expression is tail/expr compiled, earlier ones are
    ;; compiled and popped.
    (define (compile-body-seq es tail? envs emit next-label next-slot add-code)
      (if (null? (cdr es))
        (if tail?
          (compile-tail (car es) envs emit next-label next-slot add-code)
          (compile-expr (car es) envs emit next-label next-slot add-code))
        (let loop ((bs es))
          (cond
            ((null? (cdr bs))
             (if tail?
               (compile-tail (car bs) envs emit next-label next-slot add-code)
               (compile-expr (car bs) envs emit next-label next-slot add-code)))
            (else
             (compile-expr (car bs) envs emit next-label next-slot add-code)
             (emit '(pop))
             (loop (cdr bs)))))))

    ;; compile-tail : ir frame-envs emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is the result of the enclosing
    ;; lambda: applications become tail calls, the value is left on the
    ;; stack for a trailing (return).
    (define (compile-tail s envs emit next-label next-slot add-code)      (cond
        [(lexical-ref? s)
         (if (= (lexical-ref-depth s) 0)
           (emit (list 'local (lexical-ref-index s)))
           (emit (list 'ref (lexical-ref-depth s) (lexical-ref-index s))))
         (emit '(return))]
         [(toplevel-ref? s)
          (emit (list 'global (toplevel-ref-name s)))
          (emit '(return))]
         [(module-ref? s)
          (emit (list 'global 'module-ref))
          (emit (list 'const (module-ref-module s)))
          (emit (list 'const (module-ref-name s)))
          (emit (list 'tail-call 2))]
        [(symbol? s)
         (compile-expr s envs emit next-label next-slot add-code)
         (emit '(return))]
        [(or (const? s) (void? s))
         (emit (list 'const (if (const? s) (const-exp s) #f)))
         (emit '(return))]
        [(lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body-expr s)
                                               envs add-code)))
         (emit '(return))]
        [(conditional? s)
         (let ((els (conditional-alternate s)))
           (compile-expr (conditional-test s) envs emit next-label next-slot add-code)
           (let ((L (next-label)))
             (emit (list 'if-else L))
             (compile-tail (conditional-consequent s) envs emit next-label next-slot add-code)
             (emit (list 'label L))
             (if els
               (compile-tail els envs emit next-label next-slot add-code)
               (begin
                 (emit '(const #f))
                 (emit '(return))))))]
        [(seq? s)
         (compile-body s #t envs emit next-label next-slot add-code)]
        [(let? s)
         (compile-let 'let (let-names s) (let-vals s) (let-body s) #t envs emit
                      next-label next-slot add-code)]
        [(letrec? s)
         (compile-let 'letrec (letrec-names s) (letrec-vals s) (letrec-body s) #t envs emit
                      next-label next-slot add-code)]
        [(let-values? s)
         ;; call-with-values lowering: (let-values (producer) body) compiles
         ;; producer, then body as a closure applied to the values.
         (compile-expr (let-values-exp s) envs emit next-label next-slot add-code)
         (emit (list 'call 1))
         (compile-tail (let-values-body s) envs emit next-label next-slot add-code)]
         [(lexical-set? s)
          (compile-expr (lexical-set-exp s) envs emit next-label next-slot add-code)
          (let ((r (resolve-var envs (lexical-set-name s))))
            (cond ((and r (= (car r) 0))
                   (emit (list 'set-local (cadr r))))
                  (r (emit (list 'set-ref (car r) (cadr r))))
                  (else (emit (list 'store-global (lexical-set-name s))))))
          (emit '(return))]
        [(toplevel-set? s)
         (compile-expr (toplevel-set-exp s) envs emit next-label next-slot add-code)
         (emit (list 'store-global (toplevel-set-name s)))
         (emit '(return))]
        [(call? s)
         (compile-expr (call-proc s) envs emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a envs emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'tail-call (length (call-args s))))]
         [(primcall? s)
          (emit (list 'global (primcall-name s)))
          (for-each (lambda (a)
                      (compile-expr a envs emit next-label next-slot add-code))
                    (primcall-args s))
          (emit (list 'tail-call (length (primcall-args s))))]
         [(values? s)
          (emit (list 'global 'values))
          (for-each (lambda (a)
                      (compile-expr a envs emit next-label next-slot add-code))
                    (values-args s))
          (emit (list 'tail-call (length (values-args s))))]
         [(call-with-values? s)
          (emit (list 'global 'call-with-values))
          (compile-expr (cwv-producer s) envs emit next-label next-slot add-code)
          (compile-expr (cwv-consumer s) envs emit next-label next-slot add-code)
          (emit (list 'tail-call 2))]
         [(not (pair? s))
          (emit (list 'const s))
          (emit '(return))]
         [else          (error "to-bytecode: unknown expression" s)]))

    ;; compile-expr : ir frame-envs emit next-label next-slot code-add -> void
    ;; Compile an expression whose value is pushed onto the stack.
    (define (compile-expr s envs emit next-label next-slot add-code)
      (cond
        ((primitive-ref? s)
         (emit (list 'global (primitive-ref-name s))))
        ((toplevel-ref? s)
         (emit (list 'global (toplevel-ref-name s))))
        ((module-ref? s)
         (emit (list 'global 'module-ref))
         (emit (list 'const (module-ref-module s)))
         (emit (list 'const (module-ref-name s)))
         (emit (list 'call 2)))
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
         (emit (list 'const (if (const? s) (const-exp s) #f))))
        ((lambda? s)
         (emit (list 'closure (compile-lambda (lambda-formals s) (lambda-body-expr s)
                                              envs add-code))))
        ((conditional? s)
         (let ((else (conditional-alternate s)))
           (compile-expr (conditional-test s) envs emit next-label next-slot add-code)
           (let ((L1 (next-label)) (L2 (next-label)))
             (emit (list 'if-else L1))
             (compile-expr (conditional-consequent s) envs emit next-label next-slot add-code)
             (emit (list 'jump L2))
             (emit (list 'label L1))
             (if else
               (compile-expr else envs emit next-label next-slot add-code)
               (emit '(const #f)))
             (emit (list 'label L2)))))
        ((seq? s)
         (let ((es (seq->list s)))
           (if (null? es)
             (emit '(const #f))
             (let loop ((bs es))
               (if (null? (cdr bs))
                 (compile-expr (car bs) envs emit next-label next-slot add-code)
                 (begin
                   (compile-expr (car bs) envs emit next-label next-slot add-code)
                   (emit '(pop))
                   (loop (cdr bs))))))))
        ((let? s)
         (compile-let 'let (let-names s) (let-vals s) (let-body s) #f envs emit
                      next-label next-slot add-code))
        ((letrec? s)
         (compile-let 'letrec (letrec-names s) (letrec-vals s) (letrec-body s) #f envs emit
                      next-label next-slot add-code))
        ((let-values? s)
         (compile-expr (let-values-exp s) envs emit next-label next-slot add-code)
         (emit (list 'call 1)))
         ((lexical-set? s)
          (compile-expr (lexical-set-exp s) envs emit next-label next-slot add-code)
          (let ((r (resolve-var envs (lexical-set-name s))))
            (cond ((and r (= (car r) 0))
                   (emit (list 'set-local (cadr r))))
                  (r (emit (list 'set-ref (car r) (cadr r))))
                  (else (emit (list 'store-global (lexical-set-name s)))))))
        ((toplevel-set? s)
         (compile-expr (toplevel-set-exp s) envs emit next-label next-slot add-code)
         (emit (list 'store-global (toplevel-set-name s))))
        ((call? s)
         (compile-expr (call-proc s) envs emit next-label next-slot add-code)
         (for-each (lambda (a)
                     (compile-expr a envs emit next-label next-slot add-code))
                   (call-args s))
         (emit (list 'call (length (call-args s)))))
         ((primcall? s)
          (emit (list 'global (primcall-name s)))
          (for-each (lambda (a)
                      (compile-expr a envs emit next-label next-slot add-code))
                    (primcall-args s))
          (emit (list 'call (length (primcall-args s)))))
         ((values? s)
          (emit (list 'global 'values))
          (for-each (lambda (a)
                      (compile-expr a envs emit next-label next-slot add-code))
                    (values-args s))
          (emit (list 'call (length (values-args s)))))
         ((call-with-values? s)
          (emit (list 'global 'call-with-values))
          (compile-expr (cwv-producer s) envs emit next-label next-slot add-code)
          (compile-expr (cwv-consumer s) envs emit next-label next-slot add-code)
          (emit (list 'call 2)))
         ((not (pair? s))
          (emit (list 'const s)))
         (else
          (error "to-bytecode: unknown expression" s))))

    ;; compile-let : head names vals body tail? frame-envs emit next-label next-slot code-add
    (define (compile-let head names vals body tail? envs emit next-label next-slot add-code)
      ;; set-local leaves the value on the stack (the uniform setter
      ;; convention), so every binding init is followed by an explicit pop.
      (if (eq? head 'let)
        ;; let: inits evaluated in the old env (parallel bindings)
        (let* ((new-alist (fold-left (lambda (e n)
                                       (slot-env-extend e n (next-slot)))
                                     (car envs) names))
               (new-envs (cons new-alist (cdr envs))))
          (for-each (lambda (n v)
                      (compile-expr v envs emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (assq n new-alist))))
                      (emit '(pop)))
                    names vals)
          (compile-body body tail? new-envs emit next-label next-slot add-code))
        ;; letrec/letrec*: slots allocated first, inits in the new env
        (let* ((new-alist (fold-left (lambda (e n)
                                       (slot-env-extend e n (next-slot)))
                                     (car envs) names))
               (new-envs (cons new-alist (cdr envs))))
          (for-each (lambda (n v)
                      (compile-expr v new-envs emit next-label next-slot add-code)
                      (emit (list 'set-local
                                  (cdr (assq n new-alist))))
                      (emit '(pop)))
                    names vals)
          (compile-body body tail? new-envs emit next-label next-slot add-code))))

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
              (if (toplevel-define? d)
                (begin
                  ;; A define value that is itself a let/letrec (not a
                  ;; lambda) compiles against a non-empty frame env for its
                  ;; slot allocation.  A lambda value must NOT see the empty
                  ;; top-level alist: it would shift nested-lambda capture
                  ;; depths by one (compiled refs vs the runtime chain).
                  (compile-expr (toplevel-define-exp d)
                                (if (lambda? (toplevel-define-exp d)) '() '(()))
                                emit next-label next-slot add-code)
                  (emit (list 'store-global (toplevel-define-name d))))
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
        [(call tail-call)
         (and (>= (length i) 2) (integer? (cadr i)) (>= (cadr i) 0))]
        [(if-else jump)
         (and (>= (length i) 2) (member (cadr i) labels))]
        [(label) (and (>= (length i) 2) (member (cadr i) labels))]
        [(return pop) (null? (cdr i))]
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
