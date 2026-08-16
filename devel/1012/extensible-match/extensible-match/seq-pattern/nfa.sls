(library (extensible-match seq-pattern nfa)
  (export seq-pattern-expand/nfa
          ;; exports used in the expansion of list patterns
          make-instruction
          make-vm vm-run! vm-step! vm-destroy!)
  (import (rnrs (6))
          (only (srfi :1 lists)
                iota
                append-map reverse!)
          (extensible-match ast)
          (extensible-match seq-pattern common)
          (extensible-match util))

  ;; Ordered list patterns are implemented with a fast NFA simulation
  ;; using the Pike/Laurikari technique described by Russ Cox in:
  ;;
  ;; ‘Regular Expression Matching: the Virtual Machine Approach’
  ;; <https://swtch.com/~rsc/regexp/regexp2.html>
  ;;
  ;; The main difference from the Pike-style implementation is that we
  ;; have no ‘save’ instruction — every subpattern in a list can
  ;; potentially save any number of variable values, so we use an
  ;; approach more like that of Laurikari. A register set is
  ;; maintained storing the variable values accumulated so far (see
  ;; the common library); a Scheme procedure (called the action
  ;; procedure) is associated with the ‘test’ instruction (called
  ;; ‘char’ in Cox’s article) and invokes another procedure (called
  ;; the test procedure for a subpattern). The test procedure tests
  ;; that one input value from the list matches the subpattern, and
  ;; returns variable values which the subpattern wanted to save; the
  ;; action procedure destructively updates the register set with
  ;; these new values. The ‘fork’ instruction (called ‘split’ by Cox)
  ;; copies the current register set into the new thread it creates,
  ;; while the original thread and its register set is reused.
  ;;
  ;; Once the entire pattern is known to have matched, one register
  ;; set ‘wins’ according to the leftmost-longest/greedy rule, which
  ;; is basically only implicit in this code (in the same way
  ;; described by Cox). The variables are then bound to the items from
  ;; the vector. Since variable values in ellipsized patterns are
  ;; gathered in reverse order by consing successive items onto the
  ;; left of the register with values taken from successively further
  ;; right values in the input list, the gathered lists are also
  ;; reversed (destructively).
  ;;
  ;; The implementation attempts to be intelligent and reasonably
  ;; generation-friendly in its use of mutation. We try to avoid
  ;; having older objects point to newer objects, unless the cost of
  ;; repeated consing is highly likely to exceed the effort the
  ;; garbage collector needs to expend on dealing with the resulting
  ;; generational mess: thus we set the values of registers
  ;; destructively since this is likely to happen at least once for
  ;; every item in the input list. Also, one-time mutation of an
  ;; object which is likely to significantly outlive one collection is
  ;; considered okay: thus we reverse the final lists of ellipsized
  ;; patterns’ variable values destructively, rather than
  ;; functionally, because it’s probable that the match clause will do
  ;; something significant with the values and keep them around for a
  ;; while still. Lastly, mutation of fields to immediate values (or
  ;; values which are practically very likely to be immediate) is used
  ;; with reckless abandon.
  ;;
  ;; Future improvements:
  ;;
  ;; • List patterns with only one, unbounded ellipsis and a known
  ;;   (quoted) tail don’t need to use the NFA simulation: they can
  ;;   use an advance/lookahead/hare pointer to detect the end of the
  ;;   input and switch the main matching pointer (tortoise pointer)
  ;;   out of the ellipsis. This is the technique used for all list
  ;;   patterns by WCS (which has similar restrictions as syntax-rules
  ;;   regarding having only one ellipsis per list and combining tail
  ;;   and ellipsized patterns) and would likely generally be more
  ;;   efficient for this case.
  ;;
  ;; • List patterns which don’t bind any pattern variables at all can
  ;;   be completely compiled into untagged DFAs for perfect native
  ;;   matching efficiency.

  (define (seq-pattern-expand/nfa seq-pattern expand-subpat
                                  conseq alter)
    (let*-values
        (((defs test-patterns)
          ;; The first stage is common to all seq pattern
          ;; strategies; see the commentary in common.sls
          (seq-pattern->defs+test-patterns seq-pattern
                                           expand-subpat))
         ((all-vars list-vars) (test-pattern-variables test-patterns))
         ((n-registers n-tests) (values (length all-vars)
                                        (length test-patterns)))
         ((instruction-ast) (test-patterns->instruction-ast test-patterns)))
      ;; sanity check the instruction numbers to help debugging the
      ;; generated NFA program
      (assert (equal? (iota (length instruction-ast))
                      (map (lambda (ast-instr)
                             (syntax-case ast-instr ()
                               ((_ n . _) (syntax->datum #'n))))
                           instruction-ast)))
      (with-syntax (((var ...) all-vars)
                    ((var-reg ...) (iota n-registers))
                    ((list-var ...) list-vars))
        #`(let ()
            #,defs
            (define instructions
              (vector
               #,@(map ast-instruction->make-instruction instruction-ast)))
            (let* ((vm (make-vm instructions #,n-registers #,n-tests))
                   (matched
                    (vm-run! vm
                             #,(pattern-subject seq-pattern)
                             #,(eq? (seq-pattern-type seq-pattern) 'partial)
                             #,(seq-pattern-name seq-pattern)
                             #,(map (lambda (state-var)
                                      (list (seq-state-var-name state-var)
                                            (seq-state-var-init state-var)
                                            (seq-state-var-step state-var)))
                                    (seq-pattern-state-vars seq-pattern))
                             #,(seq-pattern-termination-expr seq-pattern)
                             #,(seq-pattern-ref-expr seq-pattern))))
              (vm-destroy! vm)
              (if matched
                  (let ((var (register-ref matched var-reg)) ...)
                    (let ((list-var (reverse! list-var)) ...)
                      #,conseq))
                  #,alter))))))

  ;; In the next stage the test patterns are converted into an
  ;; ‘instruction AST’ (a bad name) which corresponds to instructions
  ;; for the virtual machine which simulates a tagged NFA. These ‘AST
  ;; instructions’ are later converted into run time values
  ;; representing the instructions as they are used by the VM itself.
  ;;
  ;; ast-instruction ::= (instruction:test n proc)
  ;;                  |  (instruction:fork n continue-dest fork-dest)
  ;;                  |  (instruction:branch n dest)
  ;;                  |  (instruction:end n)
  ;;
  ;; Each ast-instruction is numbered sequentially (n); this is not
  ;; used for much more than sanity checking and debugging. test
  ;; instructions run an action procedure (see the common library),
  ;; and the thread dies if the test procedure does not succeed, or
  ;; survives with updated registers to the next interation if it
  ;; does. fork instructions tell the VM branch both to continue-dest
  ;; and fork-dest, where continue-dest is considered the more likely
  ;; of the two to succeed. branch is an unconditional branch to dest
  ;; which doesn’t fork. end tells us that we’ve found a match; the
  ;; last thread to execute an ‘end’ instruction will win.
  (define-syntax instruction:test (syntax-rules ()))
  (define-syntax instruction:fork (syntax-rules ()))
  (define-syntax instruction:branch (syntax-rules ()))
  (define-syntax instruction:end (syntax-rules ()))

  (define (test-patterns->instruction-ast test-patterns)
    (let loop ((register-offset 0)
               (instruction-idx 0)
               (instructions '())
               (more-testpats test-patterns))
      (syntax-case more-testpats (test:one test:many)
        (((test:one (var ...) matcher) . more-testpats)
         (loop (+ register-offset (length #'(var ...)))
               (+ instruction-idx 1)
               (cons
                #`(instruction:test
                   #,instruction-idx
                   #,(one-action-procedure #'matcher
                                           (length #'(var ...))
                                           register-offset))
                instructions)
               #'more-testpats))
        (((test:many (var ...) 0 #t matcher) . more-testpats)
         (loop (+ register-offset (length #'(var ...)))
               (+ instruction-idx 3)
               (cons*
                #`(instruction:branch #,(+ instruction-idx 2)
                                      #,instruction-idx)
                #`(instruction:test
                   #,(+ instruction-idx 1)
                   #,(many-action-procedure #'matcher
                                            (length #'(var ...))
                                            register-offset))
                #`(instruction:fork
                   #,instruction-idx
                   #,(+ instruction-idx 1)
                   #,(+ instruction-idx 3))
                instructions)
               #'more-testpats))
        (((test:many (var ...) 0 max matcher) . more-testpats)
         (let ((max (syntax->datum #'max)))
           (loop (+ register-offset (length #'(var ...)))
                 (+ instruction-idx (* max 2))
                 (append
                  (append-map
                   (lambda (n)
                     (list
                      #`(instruction:test
                         #,(+ instruction-idx (+ (* n 2) 1))
                         #,(many-action-procedure #'matcher
                                                  (length #'(var ...))
                                                  register-offset))
                      #`(instruction:fork
                         #,(+ instruction-idx (* n 2))
                         #,(+ instruction-idx (+ (* n 2) 1))
                         #,(+ instruction-idx (* max 2)))))
                   (reverse (iota max)))
                  instructions)
                 #'more-testpats)))
        (((test:many (var ...) min max matcher) . more-testpats)
         (let ((new-min (let ((min (syntax->datum #'min)))
                          (- min 1)))
               (new-max (let ((max (syntax->datum #'max)))
                          (if (eq? max #t)
                              #t
                              (- max 1)))))
           (loop register-offset
                 (+ instruction-idx 1)
                 (cons
                  #`(instruction:test
                     #,instruction-idx
                     #,(many-action-procedure #'matcher
                                              (length #'(var ...))
                                              register-offset))
                  instructions)
                 #`((test:many (var ...) #,new-min #,new-max matcher) . more-testpats))))
        (()
         (reverse (cons #`(instruction:end #,instruction-idx)
                        instructions))))))

  ;; These AST instructions are converted into code which, at run time
  ;; (hopefully only once at library initialization, if the Scheme
  ;; compiler is very smart), constructs records of the instruction
  ;; type.
  ;;
  ;; instruction ::= #[instruction 'test proc #f]
  ;;              |  #[instruction 'fork dest_0 dest_1]
  ;;              |  #[instruction 'branch dest #f]
  ;;              |  #[instruction 'end]
  ;;
  ;; These end up in a vector representing the NFA simulation program;
  ;; the n from the ast-instructions becomes implicit in their
  ;; position in this vector.
  (define-record-type instruction (fields type arg_0 arg_1))
  (define (test-instruction-proc i) (instruction-arg_0 i))
  (define (fork-instruction-dest_0 i) (instruction-arg_0 i))
  (define (fork-instruction-dest_1 i) (instruction-arg_1 i))
  (define (branch-instruction-dest i) (instruction-arg_0 i))

  (define (ast-instruction->make-instruction ast-instruction)
    (syntax-case ast-instruction
        (instruction:test
         instruction:fork
         instruction:branch
         instruction:end)
      ((instruction:test n proc)
       #'(make-instruction 'test proc #f))
      ((instruction:fork n a b)
       #'(make-instruction 'fork a b))
      ((instruction:branch n dest)
       #'(make-instruction 'branch dest #f))
      ((instruction:end n)
       #'(make-instruction 'end #f #f))))

  ;; Then also at run time, we run the VM.

  (define *thread-id-counter* 0)
  (define (new-thread-id)
    (let ((thread-id *thread-id-counter*))
      (set! *thread-id-counter* (+ *thread-id-counter* 1))
      thread-id))

  (define-record-type thread
    (fields (mutable pc) id (mutable registers))
    #;(protocol ; uncomment to show thread creation vs recycling
     (lambda (p)
       (lambda (pc id reg)
         (display "new thread ") (display id) (newline)
         (p pc id reg)))))

  (define-record-type vm
    (fields instructions
            ;; Vectors of the threads which will be used to test this
            ;; item and the next item
            (mutable current-threads)
            (mutable next-threads)
            ;; Bitfields used to store whether the corresponding
            ;; thread vector already has an entry for the given PC; if
            ;; bit n is set, there is already a thread ready to start
            ;; at that PC
            (mutable current-threads-pcs)
            (mutable next-threads-pcs)
            ;; Many patterns keep creating a new thread and a new
            ;; register set on every iteration. To keep the cost of
            ;; consing these down, a thread which dies has the
            ;; structure representing it and its register set moved
            ;; here; when creating a new thread, we prefer to reuse
            ;; this one (setting these fields to #f) than to actually
            ;; cons up a new one.
            (mutable dead-thread))
    (protocol
     (lambda (p)
       (lambda (instructions n-registers n-tests)
         (let ((vm (p instructions
                      (make-vector (+ n-tests 1) #f)
                      (make-vector (+ n-tests 1) #f)
                      0
                      0
                      #f)))
           (vm-add-thread! vm (make-thread 0
                                           (new-thread-id)
                                           (make-registers n-registers)))
           (vm-swap-threads! vm)
           vm)))))

  (define (vm-destroy! vm)
    (vm-current-threads-set! vm #f)
    (vm-next-threads-set! vm #f)
    (vm-dead-thread-set! vm #f))

  (define (vm-alive? vm)
    (not (zero? (vm-current-threads-pcs vm))))

  (define (vm-kill-thread! vm thread)
    (vm-dead-thread-set! vm thread))

  (define (vm-swap-threads! vm)
    (let ((current-threads (vm-current-threads vm)))
      (vm-current-threads-set! vm (vm-next-threads vm))
      (vm-current-threads-pcs-set! vm (vm-next-threads-pcs vm))
      (vector-fill! current-threads #f)
      (vm-next-threads-set! vm current-threads)
      (vm-next-threads-pcs-set! vm 0)))

  (define (vm-make-thread vm pc registers)
    (cond ((vm-dead-thread vm)
           => (lambda (thread)
                #;(begin ; uncomment to show thread creation vs recycling
                  (display "recycling thread ")
                  (display (thread-id thread))
                  (newline))
                (vm-dead-thread-set! vm #f)
                (thread-pc-set! thread pc)
                (thread-registers-set! thread registers)
                thread))
          (else
           (make-thread pc (new-thread-id) registers))))

  (define (vm-add-thread! vm thread)
    (let ((pc (thread-pc thread)))
      (when (not (bitwise-bit-set? (vm-next-threads-pcs vm) pc))
        (vm-next-threads-pcs-set! vm
                                  (bitwise-bit-set (vm-next-threads-pcs vm) pc))
        (let ((instruction (vector-ref (vm-instructions vm) pc)))
          (case (instruction-type instruction)
            ((fork)
             (thread-pc-set! thread (fork-instruction-dest_0 instruction))
             (registers-cow! (thread-registers thread))
             (vm-add-thread! vm
                             thread)
             (vm-add-thread! vm
                             (vm-make-thread
                              vm
                              (fork-instruction-dest_1 instruction)
                              (thread-registers thread))))
            ((branch)
             (thread-pc-set! thread (branch-instruction-dest instruction))
             (vm-add-thread! vm thread))
            (else
             (let ((threads (vm-next-threads vm)))
               (let loop ((idx 0))
                 (if (not (vector-ref threads idx))
                     (vector-set! threads idx thread)
                     (loop (+ idx 1)))))))))))

  (define-syntax vm-run!
    (lambda (stx)
      (syntax-case stx ()
        ((_ vm-id input partial?
            input-var ((var init step) ...) terminate? ref)
         #`(let ((vm vm-id) (input-var input))
             (define current-match #f)
             (let loop ((var init) ...)
               (cond (terminate?
                      #,@(if (syntax->datum #'partial?)
                             (list #'(vm-prune-nonfinal-threads! vm)
                                   #'(vm-step! vm ref))
                             '())
                      (let ((ft (vm-finished-thread vm)))
                        (if ft
                            (thread-registers ft)
                            current-match)))
                     ;; TODO: add a test for this:
                     ((not (vm-alive? vm)) current-match)
                     (else
                      #,(if (syntax->datum #'partial?)
                            #'(let ((maybe-match (vm-step! vm ref)))
                                (when maybe-match
                                  (set! current-match maybe-match))
                                (loop step ...))
                            #'(begin
                                (vm-step! vm ref)
                                (loop step ...)))))))))))

  (define (vm-step! vm input)
    (define maybe-match #f)
    (define instructions (vm-instructions vm))
    (vm-each-current-thread
     vm
     (lambda (thread)
       (let ((instruction
              (vector-ref instructions (thread-pc thread))))
         (case (instruction-type instruction)
           ((test)
            (cond (((test-instruction-proc instruction)
                    input (thread-registers thread))
                   => (lambda (new-registers)
                        (begin
                          ;;(write thread)(newline)
                          ;;(write input)
                          ;;(newline)(newline)

                          (thread-pc-set! thread (+ (thread-pc thread) 1))
                          (thread-registers-set! thread new-registers)
                          (vm-add-thread! vm thread))))
                  (else (vm-kill-thread! vm thread))))
           ((end)
            (set! maybe-match (thread-registers thread))
            (vm-kill-thread! vm thread))))))
    (vm-swap-threads! vm)
    (cond ((vm-finished-thread vm) => thread-registers) ; needs test
          (maybe-match maybe-match)
          (else #f)))

  (define (vm-prune-nonfinal-threads! vm)
    (vm-each-current-thread
     vm
     (lambda (thread)
       (if (eqv? (thread-pc thread)
                 (- (vector-length (vm-instructions vm)) 2))
           (vm-add-thread! vm thread))))
    (vm-swap-threads! vm))

  (define (vm-each-current-thread vm proc)
    (let ((threads (vm-current-threads vm)))
      (let loop ((idx 0))
        (cond ((and (< idx (vector-length threads))
                    (vector-ref threads idx))
               => (lambda (thread)
                    (proc thread)
                    (loop (+ idx 1))))))))

  (define (vm-finished-thread vm)
    (let ((threads (vm-current-threads vm)))
      (let loop ((idx 0))
        (if (>= idx (vector-length threads))
            #f
            (let ((thread (vector-ref threads
                                      (- (vector-length threads) 1 idx))))
              (if (and thread
                       (eq? 'end (instruction-type
                                  (vector-ref (vm-instructions vm)
                                              (thread-pc thread)))))
                  thread
                  (loop (+ idx 1)))))))))
