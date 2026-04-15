(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0))
  ) ;check-true
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f)
) ;let
(check (assq 'a '()) => #f)
(check (assq 'a '((a . 1))) => '(a . 1))
(check (assq 'a '((a) (b))) => '(a))
(check (assq 'b '((a 1) (b 2) (a 3)))
  =>
  '(b 2)
) ;check
(check (assq 'a '((a 1) (b 2) (a 3)))
  =>
  '(a 1)
) ;check
(check (assq 1 '((1 "one") (2 "two")))
  =>
  '(1 "one")
) ;check
(check (assq 3 '((1 "one") (2 "two")))
  =>
  #f
) ;check
(check (assq 'x '((x . 10) (y . 20)))
  =>
  '(x . 10)
) ;check
(check (assq 'vector
         '((symbol . 1) (vector . #(1 2 3)) (list a b c))
       ) ;assq
  =>
  '(vector . #(1 2 3))
) ;check
(check (assq 'list
         '((symbol . 1) (vector . #(1 2 3)) (list a b c))
       ) ;assq
  =>
  '(list a b c)
) ;check
(check (assq 'key
         '((key . value) (other . something))
       ) ;assq
  =>
  '(key . value)
) ;check
(check (assq 'missing
         '((key . value) (other . something))
       ) ;assq
  =>
  #f
) ;check
(let ((l '((a 1) (b 2) (c . 3))))
  (check (assq 'a l) => '(a 1))
  (check-true (eq? (assq 'a l) (list-ref l 0))
  ) ;check-true
  (check (assq 'b l) => '(b 2))
  (check (assq 'c l) => '(c . 3))
  (check (assq 'd l) => #f)
) ;let
(let ((l '((2 3) (5 7) (11 . 13))))
  (check (assv 5 l) => '(5 7))
  (check (assv 11 l) => '(11 . 13))
) ;let
(let ((l '(((a)) ((b)) ((c)))))
  (check (assoc '(a) l) => '((a)))
  (check (assq '(a) l) => #f)
  (check (assv '(a) l) => #f)
) ;let
(check-report)