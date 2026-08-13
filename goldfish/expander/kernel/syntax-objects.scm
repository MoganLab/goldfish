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

(define (stx-ctx-prune ctx phase scps)
  (stx-ctx-set ctx phase (set-subtract (stx-ctx-at ctx phase) scps)))

(define (syntax-scopes stx . maybe-phase)
  (stx-ctx-at (syntax-context stx)
              (if (null? maybe-phase) 0 (car maybe-phase))))

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
         (make-syntax form new-ctx lib))))))

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
                  (lambda (ctx ph)
                    (stx-ctx-flip (stx-ctx-add ctx ph scp-add) ph scp-flip))
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
         (wrap (lambda (x) (if (syntax? x) x (make-syntax x ctx lib)))))
    (cond
      ((pair? form)
       (cons (wrap (car form)) (wrap (cdr form))))
      ((stx-vector? form)
       (vector-map wrap form))
      (else form))))

;;; datum->syntax : syntax/context datum [phase] -> syntax

(define (datum->syntax ctx-source datum . maybe-phase)
  (let ((ctx (if (syntax? ctx-source) (syntax-context ctx-source) ctx-source))
        (lib (if (syntax? ctx-source) (syntax-library ctx-source) #f))
        (phase (if (null? maybe-phase) 0 (car maybe-phase))))
    (datum->stx-ctx ctx lib phase datum)))

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
