(import (liii check)
        (liii chez)
) ;import

(check-true (atom? 42))

(check-false (atom? '(1 2 3)))
(check-false (atom? (cons 1 2)))

(check-true (atom? #(1 2 3)))

(check-report)

