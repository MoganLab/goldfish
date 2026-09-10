;;; lib/module-registry.scm -- R7RS module registry (split from module.scm).
;;; Installed by lib/install.scm before module.scm.  A module is an
;;; exp-library plus a registry entry (exp-library . export-names);
;;; per-level instances, runtime registration tracking, and instance
;;; inlets live here.  Pure defines only (no load-time calls), so file
;;; order among the module-*.scm pieces does not matter.

;;; ------------------------------------------------------------------------
;;; Minimal module API
;;; ------------------------------------------------------------------------

;;; Unified instance table: one row per (level, name) key.
;;; Row: (key rec runtime? inlet loading?)
;;;   rec      -- (exp-library . export-names), or #f (not yet expanded)
;;;   runtime? -- the runtime module (register expression) was evaluated.
;;;               Expand-time state is populated by expand-define-library
;;;               during compilation, which may happen without ever
;;;               running (a library compiled but never evaluated, e.g. by
;;;               a compile-only driver).  A registration expression refers
;;;               to dependencies as (module-ref 'lib 'name), which
;;;               resolves against the runtime module registry -- so a
;;;               library that has expand-time state but no runtime module
;;;               must be loaded (evaluated) before its dependents can be
;;;               registered.
;;;   inlet    -- the level >= 1 expand env, or #f
;;;   loading? -- inside a load's dynamic extent (circular-load guard)
;;; One table is the whole truth about an instance; separate tables
;;; invited key skew (a record without its runtime flag and vice versa).

(define *library-instances* '())

(define (instance-row key)
  (assoc key *library-instances*))

;;; fields of a row, defaulting a missing row to all-#f
(define (instance-row-fields key)
  (let ((row (instance-row key)))
    (if row (cdr row) (list #f #f #f #f))))

(define (instance-row-set! key rec runtime? inlet loading?)
  (set! *library-instances*
        (cons (list key rec runtime? inlet loading?)
              (filter (lambda (e) (not (equal? (car e) key)))
                      *library-instances*))))

(define (registry-key level name)
  (let ((lvl (if (pair? level) (car level)
               (if (integer? level) level 0))))
    (if (or (not lvl) (= lvl 0)) name (cons lvl name))))

(define (registry-level-arg maybe-level)
  (if (pair? maybe-level) (car maybe-level) 0))

(define (runtime-registered? name . maybe-level)
  (let ((row (instance-row (registry-key (registry-level-arg maybe-level) name))))
    (and row (caddr row) #t)))

(define (runtime-registered-add! name . maybe-level)
  (let* ((key (registry-key (registry-level-arg maybe-level) name))
         (f (instance-row-fields key)))
    (instance-row-set! key (car f) #t (caddr f) (cadddr f)))
  name)

(define (library-registry-ref name . maybe-level)
  (let ((row (instance-row (registry-key (registry-level-arg maybe-level) name))))
    (and row (cadr row))))

(define (library-registry-set! name record . maybe-level)
  (let* ((key (registry-key (registry-level-arg maybe-level) name))
         (f (instance-row-fields key)))
    (instance-row-set! key record (cadr f) (caddr f) (cadddr f))))

(define (instance-inlet-ref name level)
  (let ((row (instance-row (registry-key level name))))
    (and row (cadddr row))))

(define (instance-inlet-set! name level inlet)
  (let* ((key (registry-key level name))
         (f (instance-row-fields key)))
    (instance-row-set! key (car f) (cadr f) inlet (cadddr f))))

;;; loading-guard-push! / loading-guard-pop! / instance-loading? : key -> void/bool
;;; The circular-load guard: key on *library-instances* for a load's
;;; dynamic extent (pushed/popped around load-library-in-unit! bodies),
;;; cleared on exit.

(define (loading-guard-push! key)
  (let ((f (instance-row-fields key)))
    (instance-row-set! key (car f) (cadr f) (caddr f) #t)))

(define (loading-guard-pop! key)
  (let ((f (instance-row-fields key)))
    (instance-row-set! key (car f) (cadr f) (caddr f) #f)))

(define (instance-loading? name . maybe-level)
  (let ((row (instance-row (registry-key (registry-level-arg maybe-level) name))))
    (and row (car (cddddr row)) #t)))

;;; instance-record-drop! : name -> void
;;; Drop the bare record (capture clobbers it as a side effect; see
;;; perlevel-rebuild!), keeping the row's flags -- the old split tables
;;; preserved them by construction, the unified row must do it explicitly.

(define (instance-record-drop! name)
  (let ((f (instance-row-fields name)))
    (instance-row-set! name #f (cadr f) (caddr f) (cadddr f))))

(define (make-lib-record lib exports)
  (cons lib exports))

(define (lib-record-library rec) (car rec))
(define (lib-record-exports rec) (cdr rec))
