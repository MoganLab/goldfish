(import (goldfish))
(define audit-original-load-path *load-path*)
(set! *load-path* (cons "tests/r7rs/fixtures" *load-path*))
(import (scheme base) (liii check)
        (r7rs-audit state) (r7rs-audit provider) (r7rs-audit consumer)
        (rename (prefix (only (r7rs-audit facade)
                             facade-read facade-bump hygienic-plus) audit-)
                (audit-facade-read read-alias)
                (audit-facade-bump bump-alias)
                (audit-hygienic-plus add-alias)))
(check-set-mode! 'report-failed)

;; 5.2 / 5.6: transitive providers initialize before consumers.
(check (events) => '(provider consumer))
(check initial => 12)
(check (eq? read-alias read-counter) => #t)
(check (eq? bump-alias bump!) => #t)
(check (read-alias) => 10)
(bump-alias)
(check (read-counter) => 11)

;; Macro templates retain private definition-site bindings through re-exports.
(let ((private-plus (lambda (x) 'captured)) (counter 999))
  (check (add-alias 2) => 13))

(set! *load-path* audit-original-load-path)
(check-report)
