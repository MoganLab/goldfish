;;; Bootstrap placeholder file, intentionally empty.
;;;
;;; It used to define always-true and/or/cond/case placeholders so that
;;; prelude transformers could expand before `let' existed.  The real fix
;;; is the definition order in liii/prelude.scm (`let' first): a transformer
;;; body must only mention macros defined above it.  A placeholder cond is
;;; worse than none -- it silently expands every cond to #t -- so nothing is
;;; installed here anymore.  The native source bootstrap still loads this
;;; file; keep it as the marker for that path.
