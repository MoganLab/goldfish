;;; syntax-objects.scm
;;; Phase-aware syntax objects for the scope-set expander.
;;;
;;; A syntax object is:
;;;   (make-syntax form context library)
;;; where context is a list of (phase . scopes) entries,
;;; and library is the home library for free-identifier resolution.

(define-record-type <syntax>
  (make-syntax form context library)
  syntax?
  (form    syntax-form)
  (context syntax-context)
  (library syntax-library))

;;; stx-vector? : any -> bool
;;; Container vectors only: in s7, `vector?' also holds for bytevectors,
;;; whose elements are plain bytes the expander must never recurse into
;;; (recursing would rebuild a #u8(...) literal as a plain vector).

(define (stx-vector? x)
  (and (vector? x) (not (bytevector? x))))

;;; Syntax-object contexts (phase-indexed scope sets)

(define (stx-ctx-empty) '())

(define (stx-ctx-at ctx phase)
  (let ((entry (assoc phase ctx)))
    (if entry (cdr entry) '())))

(define (stx-ctx-set ctx phase scopes)
  (let ((entry (assoc phase ctx)))
    (if entry
        (map (lambda (e)
               (if (= (car e) phase)
                   (cons phase scopes)
                   e))
             ctx)
        (cons (cons phase scopes) ctx))))

(define (stx-ctx-add ctx phase scp)
  (stx-ctx-set ctx phase (set-add (stx-ctx-at ctx phase) scp)))

(define (stx-ctx-flip ctx phase scp)
  (stx-ctx-set ctx phase (set-flip (stx-ctx-at ctx phase) scp)))

;;; *current-intro-scope* : scope/#f
;;; The current macro's introduction scope (scp_i), set by
;;; expand-macro-once while the transformer runs.  Marking happens at
;;; node-construction time (datum->syntax, template instantiation)
;;; instead of by an output-wide flip, so introduced nodes carry scp_i
;;; and input nodes (already-existing syntax) do not.

(define *current-intro-scope* #f)

(define (current-intro-scope) *current-intro-scope*)

(define (set-current-intro-scope! scp) (set! *current-intro-scope* scp))

;;; stx-ctx-mark-intro : ctx phase -> ctx
;;; Add *current-intro-scope* to phase 0 of ctx.  Template nodes created
;;; by instantiation at the current macro expansion are introduced
;;; syntax; the eager output-flip model pins scp_i on the macro use's
;;; phase (0 for ordinary, 1 for transformer-position templates -- the
;;; latter stay unmarked here, matching the eager model's single-phase
;;; flip and avoiding scope pollution across phases).  scp_i is freshly
;;; allocated per macro use and hence guaranteed absent, so this is an
;;; O(#phases) cons instead of set-add's membership scan.

(define (stx-ctx-mark-intro ctx phase)
  (if (not *current-intro-scope*)
      ctx
      (let ((entry (assoc 0 ctx)))
        (if entry
            (map (lambda (e)
                   (if (= (car e) 0)
                       (cons 0 (cons *current-intro-scope* (cdr e)))
                       e))
                 ctx)
            (cons (cons 0 (list *current-intro-scope*)) ctx)))))

;;; stx-ctx-add-then-flip : ctx phase scp-add scp-flip -> ctx
;;; ADD then FLIP in one pass.  scp-add/scp-flip are assumed freshly
;;; allocated (context-alloc-scope), hence guaranteed absent from the
;;; phase's scope set, so both ops are O(1) conses instead of the
;;; O(#scopes) membership scan in set-add/set-flip.  This is the hot
;;; path of expand-macro-once's input preprocessing.

(define (stx-ctx-add-then-flip ctx phase scp-add scp-flip)
  (let ((entry (assoc phase ctx)))
    (if entry
        (map (lambda (e)
               (if (= (car e) phase)
                   (cons phase (cons scp-flip (cons scp-add (cdr e))))
                   e))
             ctx)
        (cons (cons phase (list scp-flip scp-add)) ctx))))

;;; stx-ctx-add-unchecked : ctx phase scp -> ctx
;;; Single-scope ADD assuming scp is freshly allocated (absent): an
;;; O(#phases) cons instead of set-add's O(#scopes) membership scan.
;;; The expand-macro-once use-scope marking hot path calls this directly.

(define (stx-ctx-add-unchecked ctx phase scp)
  (let ((entry (assoc phase ctx)))
    (if entry
        (map (lambda (e)
               (if (= (car e) phase)
                   (cons phase (cons scp (cdr e)))
                   e))
             ctx)
        (cons (cons phase (list scp)) ctx))))

;;; stx-add-scope-unchecked : syntax scp [phase] -> syntax
;;; Tree-wide ADD of a freshly-allocated scope: like stx-add-scope, but
;;; O(#phases) per node instead of O(#scopes).  Safe only when scp is
;;; guaranteed absent (context-alloc-scope output).

(define (stx-add-scope-unchecked stx scp . maybe-phase)
  (stx-apply-ctx stx
                 (lambda (ctx ph) (stx-ctx-add-unchecked ctx ph scp))
                 (if (null? maybe-phase) 0 (car maybe-phase))))

(define (stx-ctx-prune ctx phase scps)
  (stx-ctx-set ctx phase (set-subtract (stx-ctx-at ctx phase) scps)))

(define (syntax-scopes stx . maybe-phase)
  (stx-ctx-at (syntax-context stx)
              (if (null? maybe-phase) 0 (car maybe-phase))))

;;; stx-apply-ctx : stx (ctx phase -> ctx) phase -> stx
;;; Apply a context transformation recursively to a syntax object.

;;; map-spine : (any -> any) list-or-improper -> list-or-improper
;;; Like map, but tolerates improper lists: the terminal atom (if not
;;; '()) is also mapped.  Dotted structures arise from dotted lambda
;;; parameter lists (R7RS 7.3).

(define (map-spine f xs)
  (cond
    ((null? xs) '())
    ((pair? xs) (cons (f (car xs)) (map-spine f (cdr xs))))
    (else (f xs))))

(define (stx-apply-ctx stx f phase)
  (if (not (syntax? stx))
      ;; Partially-wrapped tree skeleton (datum pairs whose leaves are
      ;; syntax objects, e.g. dotted tails): recurse into pairs/vectors,
      ;; leave other datums as-is -- PopSyntax's adjust-scopes tolerates
      ;; these.
      (cond
        ((pair? stx)
         (cons (stx-apply-ctx (car stx) f phase)
               (stx-apply-ctx (cdr stx) f phase)))
        ((stx-vector? stx)
         (vector-map (lambda (s) (stx-apply-ctx s f phase)) stx))
        (else stx))
      (let ((form (syntax-form stx))
            (ctx  (syntax-context stx))
            (lib  (syntax-library stx)))
        (let ((new-ctx (f ctx phase)))
          (cond
            ((pair? form)
             (make-syntax (map-spine (lambda (s) (stx-apply-ctx s f phase)) form)
                          new-ctx lib))
            ((stx-vector? form)
             (make-syntax (vector-map (lambda (s) (stx-apply-ctx s f phase)) form)
                          new-ctx lib))
            (else
             (make-syntax form new-ctx lib)))))))

(define (stx-add-scope stx scp . maybe-phase)
  (stx-apply-ctx stx
                  (lambda (ctx ph) (stx-ctx-add ctx ph scp))
                  (if (null? maybe-phase) 0 (car maybe-phase))))

(define (stx-flip-scope stx scp . maybe-phase)
  (stx-apply-ctx stx
                  (lambda (ctx ph) (stx-ctx-flip ctx ph scp))
                  (if (null? maybe-phase) 0 (car maybe-phase))))

;;; stx-add-then-flip : syntax scp-add scp-flip [phase] -> syntax
;;; Single pass applying ADD then FLIP to every node's context.  The two
;;; scope ops commute (both are set operations on the phase's scope set),
;;; so one tree traversal replaces two (expand-macro-once flips the
;;; input twice: add scp-u then flip scp-i).

(define (stx-add-then-flip stx scp-add scp-flip . maybe-phase)
  (stx-apply-ctx stx
                  (lambda (ctx ph) (stx-ctx-add-then-flip ctx ph scp-add scp-flip))
                  (if (null? maybe-phase) 0 (car maybe-phase))))

(define (stx-prune-scopes stx scps . maybe-phase)
  (stx-apply-ctx stx
                  (lambda (ctx ph) (stx-ctx-prune ctx ph scps))
                  (if (null? maybe-phase) 0 (car maybe-phase))))

;;; stx-maybe-flip : syntax scp/#f phase -> syntax
;;; Flip by scp when scp is a real scope; a #f scp (model no-scope)
;;; leaves the syntax object unchanged.  Mirrors ph-stx-maybe-flip.

(define (stx-maybe-flip stx scp phase)
  (if scp (stx-flip-scope stx scp phase) stx))

;;; stx-set-library : syntax exp-library -> syntax
;;; Recursively retarget the home library of a syntax object.

(define (stx-set-library stx lib)
  (let ((form (syntax-form stx)))
    (cond
      ((pair? form)
       (make-syntax (map-spine (lambda (s) (stx-set-library s lib)) form)
                    (syntax-context stx) lib))
      ((stx-vector? form)
       (make-syntax (vector-map (lambda (s) (stx-set-library s lib)) form)
                    (syntax-context stx) lib))
      (else
       (make-syntax form (syntax-context stx) lib)))))

;;; identifier? : any -> bool

(define (identifier? obj)
  (and (syntax? obj)
       (symbol? (syntax-form obj))))

;;; bound-identifier=? : id id [phase] -> bool

(define (bound-identifier=? id1 id2 . maybe-phase)
  (unless (and (identifier? id1) (identifier? id2))
    (error "bound-identifier=?: not identifiers" id1 id2))
  (let ((phase (if (null? maybe-phase) 0 (car maybe-phase))))
    (and (eq? (syntax-form id1) (syntax-form id2))
         (set=? (syntax-scopes id1 phase)
                (syntax-scopes id2 phase)))))

;;; syntax->datum : any -> datum

(define (syntax->datum stx)
  (cond
    ((syntax? stx)
     (let ((form (syntax-form stx)))
       (cond
         ((pair? form)
          (cons (syntax->datum (car form))
                (syntax->datum (cdr form))))
         ((stx-vector? form)
          (vector-map (lambda (x) (syntax->datum x)) form))
         (else form))))
    ((pair? stx)
     (cons (syntax->datum (car stx))
           (syntax->datum (cdr stx))))
    ((stx-vector? stx)
     (vector-map (lambda (x) (syntax->datum x)) stx))
    (else stx)))

;;; syntax-e : syntax -> datum/syntax-pair

(define (syntax-e stx)
  (unless (syntax? stx)
    (error "syntax-e: not a syntax object" stx))
  (let* ((form (syntax-form stx))
         (ctx  (syntax-context stx))
         (lib  (syntax-library stx))
         (wrap (lambda (x) (if (syntax? x)
                               x
                               (make-syntax x ctx lib)))))
    (cond
      ((pair? form)
       (cons (wrap (car form)) (wrap (cdr form))))
      ((stx-vector? form)
       (vector-map wrap form))
      (else form))))

;;; datum->syntax : syntax/context datum [phase] -> syntax

(define (datum->syntax ctx-source datum . maybe-phase)
  (let ((phase (if (null? maybe-phase) 0 (car maybe-phase))))
    (datum->stx-ctx-source ctx-source datum phase)))

(define (datum->stx-ctx-source ctx-source datum phase)
  (datum->stx-ctx (if (syntax? ctx-source) (syntax-context ctx-source) ctx-source)
                  (if (syntax? ctx-source) (syntax-library ctx-source) #f)
                  phase datum))

(define (datum->stx-ctx ctx lib phase datum)
  (unless (list? ctx)
    (error "datum->syntax: context source is neither syntax nor a scope-set context" ctx))
  (cond
    ((syntax? datum) datum)
    ((pair? datum)
     (make-syntax (map-spine (lambda (x) (datum->stx-ctx ctx lib phase x)) datum)
                  ctx lib))
    ((stx-vector? datum)
     (make-syntax (vector-map (lambda (x) (datum->stx-ctx ctx lib phase x)) datum)
                  ctx lib))
    (else
     ;; Tolerate s7-read quoted datums: inside `'(... 'x ...)'` the nested
     ;; quote survives as s7's internal #_quote object (type `syntax?'),
     ;; which our R7RS reader never produces.  Map it to the plain quote
     ;; symbol so the expander sees a uniform `(quote ...)'.  Anything with
     ;; a non-symbol/number/... s7 type in this position can only be such
     ;; an internal syntax object, so the type test needs no captured
     ;; constant and no host seam in the seed.
     (make-syntax (if (eq? (type-of datum) 'syntax?) 'quote datum) ctx lib))))


;;; generate-temporaries : syntax-list -> (list syntax)

(define (generate-temporaries lst)
  (let ((ulst (syntax->datum lst)))
    (if (list? ulst)
        (map (lambda (_)
               (make-syntax (make-fresh-name 't) (stx-ctx-empty) #f))
             ulst)
        (error "generate-temporaries: not a proper list" lst))))

;;; Library exports

(module-define! the-expander-library 'syntax? syntax?)
(module-define! the-expander-library 'make-syntax make-syntax)
(module-define! the-expander-library 'syntax-form syntax-form)
(module-define! the-expander-library 'syntax-context syntax-context)
(module-define! the-expander-library 'syntax-library syntax-library)
(module-define! the-expander-library 'syntax-scopes syntax-scopes)
(module-define! the-expander-library 'stx-ctx-empty stx-ctx-empty)
(module-define! the-expander-library 'stx-ctx-at stx-ctx-at)
(module-define! the-expander-library 'stx-ctx-set stx-ctx-set)
(module-define! the-expander-library 'stx-ctx-add stx-ctx-add)
(module-define! the-expander-library 'stx-ctx-flip stx-ctx-flip)
(module-define! the-expander-library 'stx-ctx-prune stx-ctx-prune)
(module-define! the-expander-library 'syntax->datum syntax->datum)
(module-define! the-expander-library 'syntax-e syntax-e)
(module-define! the-expander-library 'datum->syntax datum->syntax)
(module-define! the-expander-library 'identifier? identifier?)
(module-define! the-expander-library 'bound-identifier=? bound-identifier=?)
(module-define! the-expander-library 'generate-temporaries generate-temporaries)
(module-define! the-expander-library 'stx-add-scope stx-add-scope)
(module-define! the-expander-library 'stx-flip-scope stx-flip-scope)
(module-define! the-expander-library 'stx-add-then-flip stx-add-then-flip)
(module-define! the-expander-library 'stx-maybe-flip stx-maybe-flip)
(module-define! the-expander-library 'stx-prune-scopes stx-prune-scopes)
(module-define! the-expander-library 'stx-set-library stx-set-library)
(module-define! the-expander-library 'stx-add-scope-unchecked stx-add-scope-unchecked)
(module-define! the-expander-library 'current-intro-scope current-intro-scope)
(module-define! the-expander-library 'set-current-intro-scope! set-current-intro-scope!)
(module-define! the-expander-library 'stx-ctx-mark-intro stx-ctx-mark-intro)
