;;; lib/module-registry.scm -- R7RS module registry (split from module.scm).
;;; Installed by lib/install.scm before module.scm.  A module is an
;;; exp-library plus a registry entry (exp-library . export-names);
;;; per-level instances, runtime registration tracking, and instance
;;; inlets live here.  Pure defines only (no load-time calls), so file
;;; order among the module-*.scm pieces does not matter.

;;; ------------------------------------------------------------------------
;;; Minimal module API
;;; ------------------------------------------------------------------------

;;; Registry: maps module name -> (exp-library . export-names)

(define *library-registry* '())

;;; Libraries whose runtime module (the register expression) has been
;;; evaluated.  The expand-time registry (above) is populated by
;;; expand-define-library during compilation, which may happen without the
;;; runtime registration expression ever running (a library compiled but
;;; never evaluated, e.g. by a compile-only driver).  A registration
;;; expression refers to dependencies as (module-ref 'lib 'name), which
;;; resolves against the runtime module registry -- so a library that has
;;; expand-time state but no runtime module must be loaded (evaluated)
;;; before its dependents can be registered.

(define *runtime-registered-libraries* '())

(define (registry-key level name)
  (let ((lvl (if (pair? level) (car level)
               (if (integer? level) level 0))))
    (if (or (not lvl) (= lvl 0)) name (cons lvl name))))

(define (registry-level-arg maybe-level)
  (if (pair? maybe-level) (car maybe-level) 0))

(define (runtime-registered? name . maybe-level)
  (let ((key (registry-key (registry-level-arg maybe-level) name)))
    (member key *runtime-registered-libraries*)))

(define (runtime-registered-add! name . maybe-level)
  (let ((key (registry-key (registry-level-arg maybe-level) name)))
    (unless (member key *runtime-registered-libraries*)
      (set! *runtime-registered-libraries*
            (cons key *runtime-registered-libraries*))))
  name)

(define (library-registry-ref name . maybe-level)
  (let ((key (registry-key (registry-level-arg maybe-level) name)))
    (let ((entry (assoc key *library-registry*)))
      (and entry (cdr entry)))))

(define (library-registry-set! name record . maybe-level)
  (let ((key (registry-key (registry-level-arg maybe-level) name)))
    (set! *library-registry*
          (cons (cons key record)
                (filter (lambda (e) (not (equal? (car e) key)))
                        *library-registry*)))))

(define *library-instance-inlets* '())

(define (instance-inlet-ref name level)
  (let ((key (registry-key level name)))
    (let ((e (assoc key *library-instance-inlets*)))
      (and e (cdr e)))))

(define (instance-inlet-set! name level inlet)
  (let ((key (registry-key level name)))
    (set! *library-instance-inlets*
          (cons (cons key inlet)
                (filter (lambda (e) (not (equal? (car e) key)))
                        *library-instance-inlets*)))))

(define (make-lib-record lib exports)
  (cons lib exports))

(define (lib-record-library rec) (car rec))
(define (lib-record-exports rec) (cdr rec))
