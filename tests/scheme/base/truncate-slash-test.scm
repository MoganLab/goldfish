(import (liii check))
(import (liii base))
(import (scheme base))
(check-set-mode! 'report-failed)
;; truncate/
;; 测试截断除法函数的各种情况
(check (receive (q r) (truncate/ 11 3) q)
  =>
  3
) ;check
(check (receive (q r) (truncate/ 11 3) r)
  =>
  2
) ;check
(check (receive (q r) (truncate/ 11 -3) q)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ 11 -3) r)
  =>
  2
) ;check
(check (receive (q r) (truncate/ -11 3) q)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ -11 3) r)
  =>
  -2
) ;check
(check (receive (q r) (truncate/ -11 -3) q)
  =>
  3
) ;check
(check (receive (q r) (truncate/ -11 -3) r)
  =>
  -2
) ;check
(check (receive (q r) (truncate/ 10 2) q)
  =>
  5
) ;check
(check (receive (q r) (truncate/ 10 2) r)
  =>
  0
) ;check
(check (receive (q r) (truncate/ 10 -2) q)
  =>
  -5
) ;check
(check (receive (q r) (truncate/ 10 -2) r)
  =>
  0
) ;check
(check (receive (q r) (truncate/ -10 2) q)
  =>
  -5
) ;check
(check (receive (q r) (truncate/ -10 2) r)
  =>
  0
) ;check
(check (receive (q r) (truncate/ -10 -2) q)
  =>
  5
) ;check
(check (receive (q r) (truncate/ -10 -2) r)
  =>
  0
) ;check
(check (receive (q r) (truncate/ 15 4) q)
  =>
  3
) ;check
(check (receive (q r) (truncate/ 15 4) r)
  =>
  3
) ;check
(check (receive (q r) (truncate/ 15 -4) q)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ 15 -4) r)
  =>
  3
) ;check
(check (receive (q r) (truncate/ -15 4) q)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ -15 4) r)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ -15 -4) q)
  =>
  3
) ;check
(check (receive (q r) (truncate/ -15 -4) r)
  =>
  -3
) ;check
(check (receive (q r) (truncate/ 1 3) q)
  =>
  0
) ;check
(check (receive (q r) (truncate/ 1 3) r)
  =>
  1
) ;check
(check (receive (q r) (truncate/ 0 5) q)
  =>
  0
) ;check
(check (receive (q r) (truncate/ 0 5) r)
  =>
  0
) ;check
(check-catch 'division-by-zero
  (truncate/ 11 0)
) ;check-catch
(check-catch 'division-by-zero
  (truncate/ 0 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (truncate/ 1.0+1.0i 2)
) ;check-catch
(check-catch 'wrong-type-arg
  (truncate/ 5 #t)
) ;check-catch
(check-report)