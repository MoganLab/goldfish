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
      (%make-match-condition)
      &match?
      (irritants match-condition-irritants))

    (define (make-match-violation)
      (list 'match-violation))

    (define (match-violation? x)
      (and (pair? x) (eq? (car x) 'match-violation)))

    ;; ------------------------------------------------------------------
    ;; match-ellipsis? : syntax-or-datum -> boolean
    ;;   True for the ellipsis marker `...` and extended ellipses
    ;;   `(... n)`, `(... min #t)`, `(... min max)`.
    (define (match-ellipsis? x)
      (let ((f (if (syntax? x) (syntax->datum x) x)))
        (cond ((eq? f '...) #t)
              ((and (pair? f) (eq? (car f) '...)) #t)
              (else #f))))

    (define (ell? x)
      (or (eq? x '...)
          (and (syntax? x) (eq? (syntax-form x) '...))))

    ;; ------------------------------------------------------------------
    ;; Pattern-syntax registry (Racket-style, binding-backed).
    ;;
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

    (define (pattern-transformer id)
      (let ((binding (syntax-local-value id)))
        (if (and (binding? binding)
                 (eq? (binding-kind binding) 'transformer))
          (let ((proc (binding-value binding)))
            (if (procedure? proc) proc #f))
          #f)))

    ;; ------------------------------------------------------------------
    ;; Bit helpers (used by the NFA and the unordered matcher).

    (define (%bitwise-bit-set? bits n)
      (not (zero? (logand bits (ash 1 n)))))

    (define (%bitwise-bit-set bits n)
      (logior bits (ash 1 n)))

    (define (seq-range n)
      (let loop ((i 0) (acc '()))
        (if (>= i n) (reverse acc) (loop (+ i 1) (cons i acc)))))

    (define (last-elem lst) (car (reverse lst)))

    (define (drop-right lst n)
      (let ((len (length lst)))
        (let loop ((l lst) (i 0) (acc '()))
          (if (= i (- len n))
            (reverse acc)
            (loop (cdr l) (+ i 1) (cons (car l) acc))))))

    ;; ------------------------------------------------------------------
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
                   (%vm-kill-thread! vm thread)))))))        (%vm-swap-threads! vm)
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

    (define (expand-pattern pat)
      (let* ((form (if (syntax? pat) (syntax-form pat) pat))
             (d (if (syntax? pat) (syntax->datum pat) pat)))
        (cond
          ((symbol? d)
           (if (eq? d '_)
             '(wildcard)
             (list 'var d)))
          ((not (pair? d))
           (list 'quote d))
          ((symbol? (car d))
           (let ((head (car d)))
             (cond
               ((eq? head 'quote)
                (list 'quote (cadr d)))
               ((eq? head 'and)
                (cons 'seq-and (map expand-pattern (cdr form))))
               ((eq? head 'or)
                (cons 'seq-or (map expand-pattern (cdr form))))
               ((eq? head 'not)
                (list 'seq-not (expand-pattern (cadr form))))
               ((eq? head '?)
                (list 'seq-pred (cadr d)
                      (map expand-pattern (cddr form))))
               ((eq? head '=>)
                (list 'seq-proj (cadr d)
                      (map expand-pattern (cddr form))))
               ((eq? head 'cons)
                (list 'seq-cons (expand-pattern (cadr form))
                      (expand-pattern (caddr form))))
               ((eq? head 'cons*)
                (if (has-ellipsis? (cdr form))
                  (expand-cons*-ellipsis (cdr form))
                  (expand-cons*-plain (cdr form))))
               ((eq? head 'list)
                (let ((flat (flatten-sub-ellipsis (cdr form))))
                  (if (has-ellipsis? flat)
                    (expand-list-ellipsis flat)
                    (cons 'seq-list (map expand-pattern flat)))))
               ((eq? head 'vector)
                (let ((flat (flatten-sub-ellipsis (cdr form))))
                  (if (has-ellipsis? flat)
                    (expand-vector-ellipsis flat)
                    (cons 'seq-vector (map expand-pattern flat)))))
               ((eq? head 'lset)
                (list 'seq-and
                      (list 'seq-pred 'list? '())
                      (append (list 'seq 'unordered 'ls
                                    '((more ls (cdr more)))
                                    '(null? more)
                                    '(car more))
                              (expand-seq-subpats (cdr form)))))
               ((eq? head 'eof-object)
                (list 'seq-pred 'eof-object? '()))
               ((eq? head 'seq)
                (expand-seq-form 'ordered form))
               ((eq? head 'seq*)
                (expand-seq-form 'partial form))
               ((eq? head 'seq/unordered)
                (expand-seq-form 'unordered form))
               ((eq? head 'quasiquote)
                (expand-quasiquote-form (cadr form)))
               (else
                (let ((tx (if (syntax? pat)
                            (pattern-transformer (car form))
                            #f)))
                  (if tx
                    (let ((result (tx pat)))
                      (if (syntax? result)
                        (expand-pattern result)
                        (error "define-pattern-syntax: transformer must return syntax"
                               result)))
                    (if (proper-list? d)
                      (cons 'seq-list (map expand-pattern form))
                      (list 'seq-cons (expand-pattern (car form))
                            (expand-pattern (cdr form))))))))))
          (else
           (let ((flat (flatten-sub-ellipsis form)))
             (if (has-ellipsis? flat)
               (expand-list-ellipsis flat)
               (if (proper-list? d)
                 (cons 'seq-list (map expand-pattern form))
                 (list 'seq-cons (expand-pattern (car form))
                       (expand-pattern (cdr form))))))))))

    ;; has-ellipsis? : (list syntax) -> boolean
    (define (has-ellipsis? subs)
      (let loop ((more subs))
        (cond ((null? more) #f)
              ((ell? (car more)) #t)
              ((extended-ell? (car more)) #t)
              (else (loop (cdr more))))))

    ;; extended-ell? : datum-or-syntax -> boolean
    ;;   True for extended-ellipsis markers such as (... n) or (... n m).
    (define (extended-ell? x)
      (let ((f (if (syntax? x) (syntax-form x) x)))
        (and (pair? f) (ell? (car f)))))

    ;; flatten-sub-ellipsis : (list datum-or-syntax) -> (list datum-or-syntax)
    ;;   Flatten (p ...) elements into p ... so that the top-level
    ;;   ellipsis machinery handles SRFI-262 quasipattern elements.
    (define (flatten-sub-ellipsis lst)
      (apply append
             (map (lambda (e)
                    (let ((f (if (syntax? e) (syntax-form e) e)))
                      (if (and (pair? f)
                               (pair? (cdr f))
                               (null? (cddr f))
                               (ell? (cadr f)))
                        (list (car f) '...)
                        (list e))))
                  lst)))

    ;; parse-seq-subpats : (list syntax) -> (list (seq:one subpat)
    ;;                                            (seq:many min max subpat))
    ;;   Recognize ellipsis in a sequence subpattern list, following
    ;;   the grammar of SRFI-262:  subpat ... | subpat (... n) |
    ;;   subpat (... min max) where max may be #t.
    (define (parse-seq-subpats subpats)
      (define (ell-sub? x)
        (extended-ell? x))
      (let loop ((more subpats) (acc '()))
        (cond
          ((null? more) (reverse acc))
          ((and (pair? (cdr more)) (ell? (cadr more)))
           (loop (cddr more) (cons (list 'seq:many 0 #t (car more)) acc)))
          ((and (pair? (cdr more)) (ell-sub? (cadr more)))
           (let* ((f (if (syntax? (cadr more))
                       (syntax-form (cadr more))
                       (cadr more)))
                  (b (if (syntax? (cadr more))
                        (map syntax->datum (cdr f))
                        (cdr f))))
             (if (null? (cdr b))
               (loop (cddr more)
                     (cons (list 'seq:many (car b) (car b) (car more)) acc))
               (loop (cddr more)
                     (cons (list 'seq:many (car b) (cadr b) (car more)) acc)))))
          ((ell? (car more))
           (error "match: incorrect use of ellipsis in sequence pattern"))
          (else
           (loop (cdr more) (cons (list 'seq:one (car more)) acc))))))

    ;; expand-seq-subpats : (list syntax) -> (list (seq:one vars core)
    ;;                                              (seq:many vars min max core))
    (define (expand-seq-subpats subpats)
      (map (lambda (sp)
             (if (eq? (car sp) 'seq:one)
               (let ((core (expand-pattern (cadr sp))))
                 (list 'seq:one (collect-vars core) core))
               (let ((core (expand-pattern (cadddr sp))))
                 (list 'seq:many (collect-vars core)
                       (cadr sp) (caddr sp) core))))
           (parse-seq-subpats subpats)))

    ;; expand-seq-form : symbol (list datum) -> core form
    ;;   (seq name ((var init step) ...) terminate? ref subpat ...)
    (define (expand-seq-form kind form)
      (let ((name (syntax->datum (cadr form)))
            (state (map (lambda (s)
                          (map syntax->datum (syntax-form s)))
                        (syntax-form (caddr form))))
            (term (syntax->datum (cadddr form)))
            (ref (syntax->datum (car (cddddr form))))
            (subpats (cdr (cddddr form))))
        (append (list 'seq kind name state term ref)
                (expand-seq-subpats subpats))))

    ;; expand-cons*-plain : (list syntax) -> core form
    ;;   (cons* a b c) = (cons a (cons b c))
    (define (expand-cons*-plain subs)
      (if (null? (cdr subs))
        (expand-pattern (car subs))
        (list 'seq-cons (expand-pattern (car subs))
              (expand-cons*-plain (cdr subs)))))

    ;; expand-cons*-ellipsis : (list syntax) -> core form
    ;;   (cons* p ... ellipsis ... tail) compiles to a seq* whose
    ;;   elements are car-projections of the iterated list; the last
    ;;   element (tail) matches the remaining list.
    (define (expand-cons*-ellipsis subs)
      (let* ((all-but-last (drop-right subs 1))
             (tail (last-elem subs))
             (mapped (append
                      (map (lambda (p)
                             (if (or (ell? p) (extended-ell? p)) p (list '=> 'car p)))
                           all-but-last)
                      (list tail))))
        (append (list 'seq 'partial 'ls
                      '((curr ls (cdr curr)))
                      '(not (pair? curr))
                      'curr)
                (expand-seq-subpats mapped))))

    (define (expand-list-ellipsis subs)
      (append (list 'seq 'partial 'ls
                    '((curr ls (cdr curr)))
                    '(not (pair? curr))
                    'curr)
              (expand-seq-subpats
               (append
                (map (lambda (p)
                       (if (or (ell? p) (extended-ell? p)) p (list '=> 'car p)))
                     subs)
                (list (list 'quote '()))))))

    (define (expand-vector-ellipsis subs)
      (list 'seq-and
            (list 'seq-pred 'vector? '())
            (append (list 'seq 'ordered 'vec
                          '((idx 0 (+ idx 1)))
                          '(>= idx (vector-length vec))
                          '(vector-ref vec idx))
                    (expand-seq-subpats subs))))

    ;; ------------------------------------------------------------------
    ;; Quasiquote pattern expansion (SRFI-262).
    ;;
    ;; expand-quasiquote returns a *pattern datum* which is fed back
    ;; into expand-pattern; expand-seq returns a list of pattern
    ;; datums for a splicing context.

    (define (expand-quasiquote-form x)
      (expand-pattern (expand-quasiquote x 0)))

    (define (expand-quasiquote x d)
      (let ((f (syntax->datum x)))
        (cond
          ((and (pair? f) (eq? (car f) 'quasiquote) (pair? (cdr f)))
           (list 'list
                 (list 'quote 'quasiquote)
                 (expand-quasiquote (cadr f) (+ d 1))))
          ((and (pair? f) (eq? (car f) 'unquote) (pair? (cdr f)))
           (if (= d 0)
             (cadr f)
             (list 'list
                   (list 'quote 'unquote)
                   (expand-quasiquote (cadr f) (- d 1)))))
          ((and (pair? f) (eq? (car f) 'unquote))
           (if (= d 0)
             (error "quasiquote: multi-subform unquote pattern used outside splicing context")
             (error "quasiquote: malformed unquote pattern")))
          ((and (pair? f) (eq? (car f) 'unquote-splicing))
           (if (= d 0)
             (error "quasiquote: multi-subform unquote-splicing pattern used outside splicing context")
             (cons 'list
                   (cons (list 'quote 'unquote-splicing)
                         (map (lambda (q) (expand-quasiquote q (- d 1)))
                              (cdr f))))))
          ((ell? x)
           (error "quasiquote: ellipsis used in pattern outside of splicing context"))
          ((pair? f)
           (let ((rev (reverse f)))
             (cond
               ((and (pair? rev)
                     (pair? (car rev))
                     (eq? (caar rev) 'unquote)
                     (pair? (cdar rev))
                     (null? (cddar rev))
                     (= d 0))
                ;; (x ... unquote y) => proper: (list x ... y)
                ;;                 dotted:  (cons* x ... y)
                (cons (if (proper-list? f) 'list 'cons*)
                      (append (expand-seq-list (reverse (cdr rev)) d)
                              (list (cadar rev)))))
               ((and (pair? rev)
                     (pair? (cdr rev))
                     (eq? (cadr rev) 'unquote)
                     (= d 0))
                ;; (x ... unquote . y) => (cons* x ... y)
                (cons 'cons*
                      (append (expand-seq-list (reverse (cddr rev)) d)
                              (list (car rev)))))
               ((and (pair? rev)
                     (pair? (car rev))
                     (eq? (caar rev) 'unquote-splicing)
                     (pair? (cdar rev))
                     (= d 0))
                ;; (x ... unquote-splicing y) => (list x ... y ...)
                (cons 'list
                      (append (expand-seq-list (reverse (cdr rev)) d)
                              (list (list (cadar rev)
                                          '...)))))
               ((and (pair? rev)
                     (pair? (cdr rev))
                     (eq? (cadr rev) 'unquote-splicing)
                     (= d 0))
                ;; (x ... unquote-splicing . y) => (cons* x ... y)
                (cons 'cons*
                      (append (expand-seq-list (reverse (cddr rev)) d)
                              (list (car rev)))))
               ((pair? rev)
                ;; (x0 x1 ... . y) => proper: (list ...)
                ;;                dotted:  (cons* x0 x1 ... (quasiquote y))
                (if (proper-list? f)
                  (cons 'list (expand-seq-list f d))
                  (cons 'cons*
                        (append (expand-seq-list (reverse (cdr rev)) d)
                                (list (list 'quasiquote (car rev)))))))
               (else
                (list 'quote x)))))
           ((vector? f)
            (cons 'vector (expand-seq-list (vector->list f) d)))
           (else
            (list 'quote x)))))

    ;; expand-seq-list : (list datum) number -> (list pattern-datum)
    ;;   The elements of a list/vector pattern in splicing context.
    (define (expand-seq-list elems d)
      (let loop ((more elems) (acc '()))
        (if (null? more)
          (reverse acc)
          (let ((x (car more)))
            (cond
              ((and (pair? x)
                    (eq? (car x) 'unquote)
                    (= d 0)
                    (null? (cdr x)))
               (loop (cdr more) acc))
              ((and (pair? x)
                    (eq? (car x) 'unquote)
                    (= d 0)
                    (pair? (cdr x)))
               (loop (cdr more)
                     (append (reverse (cdr x)) acc)))
              ((and (pair? x)
                    (eq? (car x) 'unquote-splicing)
                    (= d 0)
                    (null? (cdr x)))
               (loop (cdr more) acc))
              ((and (pair? x)
                    (eq? (car x) 'unquote-splicing)
                    (= d 0)
                    (pair? (cdr x))
                    (null? (cddr x)))
               (let ((y (cadr x)))
                 (if (symbol? y)
                   (loop (cdr more) (cons '... (cons y acc)))
                   (error "unquote-splicing: only identifiers can be used with unquote-splicing"
                          y))))
              ((and (pair? x) (eq? (car x) 'unquote-splicing) (= d 0))
               (error "unquote-splicing: malformed unquote-splicing in pattern"))
              (else
               (loop (cdr more)
                     (cons (if (or (ell? x) (and (pair? x) (ell? (car x))))
                             x
                             (expand-quasiquote x d))
                           acc))))))))

    ;; ------------------------------------------------------------------
    ;; Code generation.
    ;;
    ;; gen* : pat (sexp subject-expr) (sexp fail) (sexp success)
    ;;        (list binds) -> (values code binds)
    ;;   On success the generated code evaluates `success' with the
    ;;   pattern variables bound to hygienic temporaries (as recorded
    ;;   in binds); on failure it evaluates (fail).

    (define (gen* pat subject fail success binds)
      (cond
        ((eq? (car pat) 'wildcard)
         (values success binds))
        ((eq? (car pat) 'var)
         (let* ((user (cadr pat))
                (entry (assq user binds))
                (tmp (if entry (cdr entry)
                         (car (generate-temporaries (list user))))))
           (values `(let ((,tmp ,subject)) ,success)
                   (if entry binds (cons (cons user tmp) binds)))))
        ((eq? (car pat) 'quote)
         (values `(if (equal? ,subject ',(cadr pat)) ,success ,fail)
                 binds))
        ((eq? (car pat) 'seq-list)
         (gen-list* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-cons)
         (let ((a (cadr pat)) (d (caddr pat)))
           (let*-values (((d-code d-binds)
                          (gen* d `(cdr ,subject) fail success binds))
                         ((a-code a-binds)
                          (gen* a `(car ,subject) fail d-code d-binds)))
             (values `(if (pair? ,subject) ,a-code ,fail) a-binds))))
        ((eq? (car pat) 'seq-vector)
         (gen-vector* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-and)
         (let loop ((subs (cdr pat)) (s success) (b binds))
           (if (null? subs)
             (values s b)
             (let*-values (((code binds2) (gen* (car subs) subject fail s b)))
               (loop (cdr subs) code binds2)))))
        ((eq? (car pat) 'seq-or)
         (gen-or* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-not)
         (gen-not* (cadr pat) subject fail success binds))
        ((eq? (car pat) 'seq-pred)
         (let ((proc (cadr pat)) (subs (caddr pat)))
           (if (null? subs)
             (values `(if (,proc ,subject) ,success ,fail) binds)
             (gen-and-pred* proc subs subject fail success binds))))
        ((eq? (car pat) 'seq-proj)
         (gen-proj* (cadr pat) (caddr pat) subject fail success binds))
        ((eq? (car pat) 'seq)
         (if (eq? (cadr pat) 'unordered)
           (gen-unordered* (caddr pat) (cadddr pat)
                           (car (cddddr pat)) (cadr (cddddr pat))
                           (cddr (cddddr pat))
                           subject fail success binds)
           (gen-seq* (cadr pat) (caddr pat) (cadddr pat)
                     (car (cddddr pat)) (cadr (cddddr pat))
                     (cddr (cddddr pat))
                     subject fail success binds)))
        (else (error "match: unknown pattern" pat))))

    (define (gen-and-pred* proc subs subject fail success binds)
      (if (null? subs)
        (values `(if (,proc ,subject) ,success ,fail) binds)
        (let loop ((ss subs) (s success) (b binds))
          (if (null? ss)
            (values `(if (,proc ,subject) ,s ,fail) b)
            (let*-values (((code binds2) (gen* (car ss) subject fail s b)))
              (loop (cdr ss) code binds2))))))

    (define (gen-list* subs subject fail success binds)
      (if (null? subs)
        (values `(if (null? ,subject) ,success ,fail) binds)
        (let ((head (car subs)))
          (let*-values (((rest-code rest-binds)
                         (gen-list* (cdr subs) `(cdr ,subject) fail success binds))
                        ((head-code head-binds)
                         (gen* head `(car ,subject) fail rest-code rest-binds)))
            (values `(if (pair? ,subject) ,head-code ,fail) head-binds)))))

    (define (gen-vector* subs subject fail success binds)
      (if (null? subs)
        (values `(if (and (vector? ,subject) (zero? (vector-length ,subject)))
                     ,success ,fail)
                binds)
        (let*-values (((elems-code elems-binds)
                       (gen-vector-elems* subs subject 0 fail success binds)))
          (values `(if (and (vector? ,subject)
                            (= (vector-length ,subject) ,(length subs)))
                       ,elems-code ,fail)
                  elems-binds))))

    (define (gen-vector-elems* subs subject idx fail success binds)
      (if (null? subs)
        (values success binds)
        (let*-values (((rest-code rest-binds)
                       (gen-vector-elems* (cdr subs) subject (+ idx 1)
                                          fail success binds))
                      ((head-code head-binds)
                       (gen* (car subs) `(vector-ref ,subject ,idx)
                             fail rest-code rest-binds)))
          (values head-code head-binds))))

    (define (gen-or* pats subject fail success binds)
      (cond
        ((null? pats) (values fail binds))
        ((null? (cdr pats)) (gen* (car pats) subject fail success binds))
        (else
         (let*-values (((rest-code rest-binds)
                        (gen-or* (cdr pats) subject fail success binds)))
           (let ((rest-fail (car (generate-temporaries (list 'or-fail)))))
             (let*-values (((first-code first-binds)
                            (gen* (car pats) subject `(,rest-fail) success
                                  rest-binds)))
               (values `(let ((,rest-fail (lambda () ,rest-code)))
                          ,first-code)
                        first-binds)))))))

    (define (gen-not* sub subject fail success binds)
      (let ((inner-ok (car (generate-temporaries (list 'not-ok)))))
        (let*-values (((sub-code sub-binds)
                       (gen* sub subject success `(,inner-ok) binds)))
          (values `(let ((,inner-ok (lambda () ,fail))) ,sub-code)
                  sub-binds))))

    (define (gen-proj* proc subs subject fail success binds)
      (if (null? subs)
        (let ((r (car (generate-temporaries (list 'proj-res)))))
          (values `(let ((,r (,proc ,subject))) ,success) binds))
        (let ((vals (map (lambda (_)
                           (car (generate-temporaries (list 'proj-val))))
                         subs)))
          (let loop ((ss subs) (vs vals) (s success) (b binds))
            (if (null? ss)
              (values `(call-with-values (lambda () (,proc ,subject))
                                         (lambda ,vals ,s))
                      b)
              (let*-values (((c b2) (gen* (car ss) (car vs) fail s b)))
                (loop (cdr ss) (cdr vs) c b2)))))))

    ;; gen-instructions : (list test-pattern) (list action-id) -> (list form)
    ;;   Compile the sequence of test patterns into NFA instructions.
    ;;   test-pattern ::= (seq:one (vars) core) | (seq:many (vars) min max core)
    (define (gen-instructions tests action-ids)
      (define (many-0-inf atp a-id)
        (list (list '%make-instruction ''fork (+ atp 1) (+ atp 3))
              (list '%make-instruction ''test a-id '#f)
              (list '%make-instruction ''branch atp '#f)))
      (define (many-0-max atp max a-id)
        (let loop ((k 0) (acc '()))
          (if (>= k max)
            acc
            (loop (+ k 1)
                  (append acc
                          (list (list '%make-instruction ''fork (+ atp (* k 2) 1)
                                      (+ atp (* max 2)))
                                (list '%make-instruction ''test a-id '#f)))))))
      (let loop ((more tests) (aids action-ids) (atp 0) (acc '()))
        (if (null? more)
          (append acc (list (list '%make-instruction ''end '#f '#f)))
          (let* ((tp (car more))
                 (a-id (car aids)))
            (if (eq? (car tp) 'seq:one)
              (loop (cdr more) (cdr aids) (+ atp 1)
                    (append acc
                            (list (list '%make-instruction ''test a-id '#f))))
              (let ((min (caddr tp)) (max (cadddr tp)))
                (cond
                  ((and (= min 0) (eq? max #t))
                   (loop (cdr more) (cdr aids) (+ atp 3)
                         (append acc (many-0-inf atp a-id))))
                  ((= min 0)
                   (loop (cdr more) (cdr aids) (+ atp (* max 2))
                         (append acc (many-0-max atp max a-id))))
                  ((eq? max #t)
                   (loop (cdr more) (cdr aids) (+ atp 1 min)
                         (append acc
                                 (append (many-0-inf (+ atp min) a-id)
                                         (make-list min
                                                    (list '%make-instruction ''test a-id '#f))))))
                   (else
                    (let ((max* (- max min)))
                      (loop (cdr more) (cdr aids) (+ atp (* min 1) (* max* 2))
                            (append acc
                                    (append (make-list min
                                                       (list '%make-instruction ''test a-id '#f))
                                            (many-0-max (+ atp min) max* a-id)))))))))))))

    ;; gen-seq* : symbol symbol (list state) datum datum
    ;;            (list test-pattern) subject fail success binds
    ;;            -> (values code binds)
    ;;   NFA-based ordered/partial sequence matching.
    (define (gen-seq* kind name state term ref tests subject fail success binds)
      (let* ((all-vars (apply append
                              (map (lambda (tp) (cadr tp)) tests)))
             (list-vars (apply append
                               (map (lambda (tp)
                                      (if (eq? (car tp) 'seq:many)
                                        (cadr tp) '()))
                                    tests)))
             (tmp-vars (map (lambda (v)
                              (let ((entry (assq v binds))) (if entry (cdr entry) (car (generate-temporaries (list v))))))
                            all-vars))
             (binds0 (append (map cons all-vars tmp-vars) binds))
             (n-reg (length all-vars))
             (n-tests (length tests))
             (test-ids (map (lambda (_)
                              (car (generate-temporaries (list 't))))
                            tests))
             (action-ids (map (lambda (_)
                                (car (generate-temporaries (list 'a))))
                              tests))
             (test-bindings
              (map (lambda (tp tid)
                     (let* ((core (last-elem tp))
                            (sub-vars (cadr tp))
                            (sub-tmps (map (lambda (v)
                                             (cdr (assq v binds0)))
                                           sub-vars)))
                       (call-with-values
                         (lambda ()
                           (gen* core 'input '#f
                                 (cons 'list (cons #t sub-tmps))
                                 binds0))
                         (lambda (code ignored)
                           (list tid `(lambda (input) ,code))))))
                   tests test-ids))
             (action-bindings
              (let loop ((more tests) (tids test-ids) (aids action-ids)
                         (off 0) (acc '()))
                (if (null? more)
                  (reverse acc)
                  (let* ((tp (car more))
                         (test-id (car tids))
                         (action-id (car aids))
                         (n-vars (length (cadr tp)))
                         (is-many (eq? (car tp) 'seq:many))
                         (updates
                          (apply append (map
                                         (lambda (i)
                                           (let ((idx (+ off i)))
                                             (if is-many
                                               (list idx
                                                     `(cons (list-ref vals ,(+ i 1))
                                                            (%register-ref regs ,idx)))
                                               (list idx
                                                     `(list-ref vals ,(+ i 1))))))
                                         (seq-range n-vars)))))
                    (loop (cdr more) (cdr tids) (cdr aids) (+ off n-vars)
                          (cons
                           (list action-id
                                 `(lambda (input regs)
                                    (let ((vals (,test-id input)))
                                      (if vals
                                        (%registers-set! regs ,@updates)
                                        #f))))
                           acc))))))
             (instructions (cons 'vector
                                 (gen-instructions tests action-ids)))
             (reg-binds
              (let loop ((i 0) (vs all-vars) (tmps tmp-vars) (acc '()))
                (if (null? vs)
                  (reverse acc)
                  (loop (+ i 1) (cdr vs) (cdr tmps)
                        (cons (list (car tmps)
                                    (if (memq (car vs) list-vars)
                                      `(reverse (%register-ref result ,i))
                                      `(%register-ref result ,i)))
                              acc)))))
             (partial? (eq? kind 'partial))
             (run-code
              `(let ((,name ,subject))
                 (let ((current-match #f))
                   (let loop ,(map (lambda (s) (list (car s) (cadr s)))
                                   state)
                     (cond
                       (,term
                        ,@(if partial?
                              (list (list 'begin
                                          (list '%vm-prune-nonfinal-threads!
                                                'vm)
                                          (list '%vm-step! 'vm ref)))
                              '())
                        (let ((ft (%vm-finished-thread vm)))
                          (if ft (%thread-registers ft) current-match)))
                       ((not (%vm-alive? vm)) current-match)
                       (else
                        ,(if partial?
                             `(let ((maybe-match (%vm-step! vm ,ref)))
                                (when maybe-match
                                  (set! current-match maybe-match))
                                (loop ,@(map (lambda (s) (caddr s)) state)))
                             `(begin
                                (%vm-step! vm ,ref)
                                (loop ,@(map (lambda (s) (caddr s)) state)))))))))))
        (values
         `(let* ,(append test-bindings action-bindings)
            (let ((instructions ,instructions))
              (let ((vm (%make-vm instructions ,n-reg ,n-tests)))
                (let ((result ,run-code))
                  (%vm-destroy! vm)
                  (if result
                    (let ,reg-binds
                      ,success)
                    ,fail)))))
         binds0)))

    ;; gen-unordered* : like gen-seq* but with backtracking search
    ;;   (used by seq/unordered and lset).
    (define (gen-unordered* name state term ref tests subject fail success binds)
      (let* (             (rest? (and (not (null? tests))
                         (let ((last (last-elem tests)))
                           (and (eq? (car last) 'seq:many)
                                (= (caddr last) 0)
                                (eq? (cadddr last) #t)))))
             (fixed (if rest? (drop-right tests 1) tests))
             (all-vars (apply append
                              (map (lambda (tp) (cadr tp)) tests)))
             (list-vars (apply append
                               (map (lambda (tp)
                                      (if (eq? (car tp) 'seq:many)
                                        (cadr tp) '()))
                                    tests)))
             (tmp-vars (map (lambda (v)
                              (let ((entry (assq v binds))) (if entry (cdr entry) (car (generate-temporaries (list v))))))
                            all-vars))
             (binds0 (append (map cons all-vars tmp-vars) binds))
             (n-reg (length all-vars))
             (n-tests (length fixed))
             (test-ids (map (lambda (_)
                              (car (generate-temporaries (list 't))))
                            fixed))
             (action-ids (map (lambda (_)
                                (car (generate-temporaries (list 'a))))
                              fixed))
             (rest-action-id (if rest?
                               (car (generate-temporaries (list 'a)))
                               #f))
             (rest-test-id (if rest? (car (generate-temporaries (list 'r))) #f))
             (rest-tp (if rest? (last-elem tests) #f))
             (rest-vars (if rest? (cadr rest-tp) '()))
             (rest-offset (if rest? (- (length all-vars) (length rest-vars)) 0))
             (rest-test-binding
              (if rest?
                (let* ((core (last-elem rest-tp))
                       (sub-tmps (map (lambda (v)
                                        (cdr (assq v binds0)))
                                      rest-vars)))
                  (call-with-values
                    (lambda ()
                      (gen* core 'input '#f
                            (cons 'list (cons #t sub-tmps))
                            binds0))
                    (lambda (code ignored)
                      (list rest-test-id `(lambda (input) ,code)))))
                '()))
             (test-bindings
              (map (lambda (tp tid)
                     (let* ((core (last-elem tp))
                            (sub-vars (cadr tp))
                            (sub-tmps (map (lambda (v)
                                             (cdr (assq v binds0)))
                                           sub-vars)))
                       (call-with-values
                         (lambda ()
                           (gen* core 'input '#f
                                 (cons 'list (cons #t sub-tmps))
                                 binds0))
                         (lambda (code ignored)
                           (list tid `(lambda (input) ,code))))))
                   fixed test-ids))
             (action-bindings
              (let loop ((more fixed) (tids test-ids) (aids action-ids)
                         (off 0) (acc '()))
                (if (null? more)
                  (reverse acc)
                  (let* ((tp (car more))
                         (test-id (car tids))
                         (action-id (car aids))
                         (n-vars (length (cadr tp)))
                         (updates
                          (apply append (map
                                         (lambda (i)
                                           (let ((idx (+ off i)))
                                             (list idx `(list-ref vals ,(+ i 1)))))
                                         (seq-range n-vars)))))
                    (loop (cdr more) (cdr tids) (cdr aids) (+ off n-vars)
                          (cons
                           (list action-id
                                 `(lambda (input regs)
                                    (let ((vals (,test-id input)))
                                      (if vals
                                        (%registers-set! regs ,@updates)
                                        #f))))
                           acc))))))
             (all-matched-bits (if (= n-tests 0) 0
                                 (let loop ((i 0) (bits 0))
                                   (if (>= i n-tests)
                                     bits
                                     (loop (+ i 1)
                                           (logior 1 (ash bits 1)))))))
             (reg-binds
              (let loop ((i 0) (vs all-vars) (tmps tmp-vars) (acc '()))
                (if (null? vs)
                  (reverse acc)
                  (loop (+ i 1) (cdr vs) (cdr tmps)
                        (cons (list (car tmps)
                                    (if (memq (car vs) list-vars)
                                      `(reverse (%register-ref registers ,i))
                                      `(%register-ref registers ,i)))
                              acc)))))
              (run-code
               (let* ((steps (map (lambda (s) (caddr s)) state))
                      (pl-idx (car (generate-temporaries (list 'pl-idx))))
                      (pattern-loop
                       (lambda (current-value)
                         `(let pattern-loop ((,pl-idx 0))
                            (cond
                              ((>= ,pl-idx ,n-tests)
                               ,(if rest?
                                    `(let ((new-regs
                                            (,(if rest? rest-action-id #f)
                                             ,current-value registers)))
                                       (if new-regs
                                         (begin
                                           (%registers-cow! registers)
                                           (loop ,@steps
                                                 backtracking-point
                                                 new-regs
                                                 matched-patterns))
                                         (if backtracking-point
                                           (backtracking-point)
                                           ,fail)))
                                    `(if backtracking-point
                                       (backtracking-point)
                                       ,fail)))
                              ((%bitwise-bit-set? matched-patterns ,pl-idx)
                               (pattern-loop (+ ,pl-idx 1)))
                              (else
                               (let ((new-regs
                                      ((vector-ref action-procs ,pl-idx)
                                       ,current-value registers)))
                                 (if new-regs
                                   (begin
                                     (%registers-cow! registers)
                                     (loop ,@steps
                                           (lambda () (pattern-loop (+ ,pl-idx 1)))
                                           new-regs
                                           (%bitwise-bit-set matched-patterns ,pl-idx)))
                                   (pattern-loop (+ ,pl-idx 1))))))))))
                 `(let ((,name ,subject))
                    (let loop ,(append
                                (map (lambda (s) (list (car s) (cadr s))) state)
                                (list (list 'backtracking-point '#f)
                                      (list 'registers
                                            `(%make-registers ,n-reg))
                                      (list 'matched-patterns 0)))
                      (if ,term

                       (if (= matched-patterns ,all-matched-bits)
                         (let ,reg-binds
                           ,success)
                         ,fail)
                       (let ((current-value ,ref))
                         ,(pattern-loop 'current-value))))))))
          (values
           `(let* ,(append test-bindings
                           action-bindings)
              ,@(if rest?
                    (list (list 'define rest-test-id
                                (cadr rest-test-binding))
                          (list 'define rest-action-id
                                `(lambda (input regs)
                                   (let ((vals (,rest-test-id input)))
                                     (if vals
                                       (let loop ((i 0) (acc '()))
                                         (if (>= i ,(length rest-vars))
                                           (apply %registers-set! regs (reverse acc))
                                           (loop (+ i 1)
                                                 (cons (cons (list-ref vals (+ i 1))
                                                             (%register-ref regs (+ ,rest-offset i)))
                                                       (cons (+ ,rest-offset i) acc)))))
                                       #f)))))
                    '())
              (let ((action-procs (vector ,@action-ids)))
                ,run-code))
           binds0)))

    ;; rename-body : (list (user . temp)) (list sexp body) -> (list sexp)
    (define (rename-body binds body)
      (define (ren expr)
        (cond
          ((symbol? expr)
           (let ((entry (assq expr binds)))
             (if entry (cdr entry) expr)))
          ((pair? expr)
           (cons (ren (car expr)) (ren (cdr expr))))
          ((vector? expr)
           (vector-map ren expr))
          (else expr)))
      (map ren body))

    ;; collect-vars : pat -> (list symbol)
    (define (collect-vars pat)
      (cond
        ((eq? (car pat) 'var) (list (cadr pat)))
        ((memq (car pat) '(seq-list seq-vector seq-and))
         (apply append (map collect-vars (cdr pat))))
        ((eq? (car pat) 'seq-cons)
         (append (collect-vars (cadr pat)) (collect-vars (caddr pat))))
        ((eq? (car pat) 'seq-not)
         (collect-vars (cadr pat)))
        ((memq (car pat) '(seq-pred seq-proj))
         (apply append (map collect-vars (caddr pat))))
        ((eq? (car pat) 'seq)
         (apply append (map (lambda (tp) (cadr tp))
                            (cddr (cddddr pat)))))
        ((eq? (car pat) 'seq-or)
         (if (null? (cdr pat))
           '()
           (let ((first (collect-vars (cadr pat))))
             (for-each (lambda (b)
                         (if (not (equal? (collect-vars b) first))
                           (error "match: or branches bind different variables")))
                       (cddr pat))
             first)))
        (else
         '())))

    ;; compile-pats-gen* : (list datum-pat) (list symbol args)
    ;;                     (list body) (sexp fail) -> sexp
    (define (compile-pats-gen* pats args body fail)
      (let* ((cores (map (lambda (p) (expand-pattern p)) pats))
             (vars (apply append (map collect-vars cores)))
             (binds (map (lambda (v)
                           (cons v (car (generate-temporaries (list v)))))
                         vars))
             (renamed-body (rename-body binds body))
             (success `(begin ,@renamed-body)))
        (let loop ((cs cores) (as args) (s success) (b binds))
          (if (null? cs)
            s
            (let*-values (((code new-binds)
                           (gen* (car cs) (car as) fail s b)))
              (loop (cdr cs) (cdr as) code new-binds))))))

    ;; ------------------------------------------------------------------
    ;; Public syntax.
    ;;
    ;; match : expr clause ... -> value
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
    (define (compile-one-clause cl-syntax next-code args)
      (let* ((cl (syntax-form cl-syntax))
             (pat-datum (syntax->datum (car cl)))
             (pats (if (list? pat-datum) pat-datum (list pat-datum)))
             (body (map syntax->datum (cdr cl))))
        (compile-pats-gen* pats args body next-code)))

    (define (compile-group arity group-clauses args)
      (letrec ((build-chain
                (lambda (cls)
                  (if (null? (cdr cls))
                    (compile-one-clause (car cls)
                                        '(raise (make-match-violation))
                                        args)
                    (let ((next-code (build-chain (cdr cls))))
                      (compile-one-clause (car cls) next-code args))))))
        (build-chain (reverse group-clauses))))

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


