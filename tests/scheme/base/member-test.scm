(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
(check-catch 'wrong-type-arg
  (member 0 "text")
) ;check-catch
(check (member 2 '(1 2 3)) => '(2 3))
(check (member 0 '(1 2 3)) => #f)
(check (member 0 '()) => #f)
(check (member "1" '(0 "1" 2 3))
  =>
  '("1" 2 3)
) ;check
(check (member '(1 . 2) '(0 (1 . 2) 3))
  =>
  '((1 . 2) 3)
) ;check
(check (member '(1 2) '(0 (1 2) 3))
  =>
  '((1 2) 3)
) ;check
(check-report)