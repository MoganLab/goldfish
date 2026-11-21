;;
;; (liii goldcheck analysis) foundation tests.

(import (scheme base) (liii check) (liii goldcheck analysis))

(check-set-mode! 'report-failed)

(define fixture-root
  "tests/resources"
) ;define

;; Simple and renamed exports.
(let ((resolved (resolve-library '(analysis first) (list fixture-root))))
  (check (car resolved) => 'resolved)
  (check-true (pair? (member 'local-name (caddr resolved))))
) ;let

(let ((resolved (resolve-library '(analysis reexport) (list fixture-root))))
  (check (car resolved) => 'resolved)
  (check-true (pair? (member 'external-name (caddr resolved))))
) ;let

;; Re-export ownership.
(check (car (reexport-owner '(analysis reexport) 'external-name (list fixture-root)))
  => 'owner
) ;check
(check (car (reexport-owner '(analysis ambiguous) 'shared-name (list fixture-root)))
  => 'indeterminate
) ;check

;; Import set normalization.
(let ((m (normalize-import-mapping
           '(only (analysis first) local-name)
           (list fixture-root)
         ) ;normalize
      ) ;m
     ) ;
  (check m => '((local-name . local-name)))
) ;let

(let ((m (normalize-import-mapping
           '(except (analysis first) local-name)
           (list fixture-root)
         ) ;normalize
      ) ;m
     ) ;
  (check-true (pair? m))
  (check-true (not (assq 'local-name m)))
) ;let

(let ((m (normalize-import-mapping
           '(prefix (analysis first) pre-)
           (list fixture-root)
         ) ;normalize
      ) ;m
     ) ;
  (check-true (pair? m))
  (check-true (pair? (assq 'pre-local-name m)))
) ;let

(let ((m (normalize-import-mapping
           '(rename (analysis first) (local-name renamed-name))
           (list fixture-root)
         ) ;normalize
      ) ;m
     ) ;
  (check-true (pair? (assq 'renamed-name m)))
) ;let

;; Procedure arity descriptors.
(check (procedure-arity '(analysis first) 'local-name (list fixture-root))
  => '(fixed 1 1)
) ;check
(check (procedure-arity '(analysis first) 'optional-name (list fixture-root))
  => '(optional 1 2 #t)
) ;check
(check (procedure-arity '(analysis first) 'rest-name (list fixture-root))
  => '(rest 1)
) ;check
(check (procedure-arity '(analysis first) 'keyword-name (list fixture-root))
  => '(keyword 1 1 #t)
) ;check

(check-report)
