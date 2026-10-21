(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii tui component)
  (liii pi tui header)
) ;import

(check-set-mode! 'report)

(let* ((hdr (make-pi-header))
       (lines (component-render hdr 80))
       (plain-lines (map strip-ansi lines))
      ) ;let*
  (check (component? hdr) => #t)
  (check (length lines) => 6)
  (check (list-ref plain-lines 0) => "")
  (check-true (string-contains? (list-ref plain-lines 1) (string-append " gf v" (version))))
  (check-true (string-contains? (list-ref plain-lines 2) "escape interrupt"))
  (check-true (string-contains? (list-ref plain-lines 2) "ctrl+c/ctrl+d clear/exit"))
  (check-true (string-contains? (list-ref plain-lines 2) "/ commands"))
  (check-true (string-contains? (list-ref plain-lines 2) "! bash"))
  (check-false (string-contains? (list-ref plain-lines 2) "ctrl+o"))
  (check (list-ref plain-lines 3) => "")
  (check-true (string-contains? (list-ref plain-lines 4) "Goldfish Scheme Agent can explain its own features"))
  (check (list-ref plain-lines 5) => "")
) ;let*

(let* ((hdr (make-pi-header :version "1.2.3"))
       (lines (component-render hdr 80))
       (plain-lines (map strip-ansi lines))
      ) ;let*
  (check-true (string-contains? (list-ref plain-lines 1) " gf v1.2.3"))
) ;let*

(check-report)
