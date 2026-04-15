(import (srfi srfi-9) (srfi srfi-78))

(check-set-mode! 'report-failed)

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr)
) ;define-record-type

(check (pare? (kons 1 2)) => #t)

(check (pare? (cons 1 2)) => #f)

(check (kar (kons 1 2)) => 1)

(check (kdr (kons 1 2)) => 2)

(check (let ((k (kons 1 2)))
         (set-kar! k 3)
         (kar k)
       ) ;let
  =>
  3
) ;check

(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age)
) ;define-record-type

(check (person? (make-person "Da" 3))
  =>
  #t
) ;check
(check (get-age (make-person "Da" 3))
  =>
  3
) ;check
(check (get-name (make-person "Da" 3))
  =>
  "Da"
) ;check
(check (let ((da (make-person "Da" 3)))
         (set-name! da "Darcy")
         (get-name da)
       ) ;let
  =>
  "Darcy"
) ;check

(check-report)
(if (check-failed?) (exit -1))
