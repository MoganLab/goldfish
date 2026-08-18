;; Regression: datum->syntax with a USE-SITE syntax object attaches the
;; use-site library, so generated free identifiers resolve at the use site
;; (as in Guile/Racket).  A macro that must generate an identifier bound
;; only in its own library (e.g. define-values, imported from (scheme base))
;; must splice in a definition-site identifier via (quote-syntax ...) for
;; the head.  See match.scm match-define-values.
(import (liii check)
        (goldfish repro-hygiene))

(check (gen-let) => 1)

(gen-defval2)
(check c => 3)
(check d => 4)
