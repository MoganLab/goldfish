(import (liii json schema budget) (liii check) (scheme base))
(check-set-mode! 'report-failed)

(define (mentions-budget? text)
  (let ((probe "budget exceeded"))
    (let loop
      ((i 0))
      (cond
       ((> (+ i (string-length probe)) (string-length text)) #f)
       ((string=? (substring text i (+ i (string-length probe))) probe) #t)
       (else (loop (+ i 1)))
      ) ;cond
    ) ;let
  ) ;let
) ;define

;; 1. A budget records its configured limits.

(define budget (make-resource-budget 10 100 1000))
(check-true (resource-budget? budget))
(check-false (resource-budget? 'not-a-budget))
(check (budget-max-depth budget) => 10)
(check (budget-max-steps budget) => 100)
(check (budget-max-references budget) => 1000)

;; 2. Counters start at zero and are incremented by the entry points.
(check (budget-depth budget) => 0)
(check (budget-steps budget) => 0)
(check (budget-references budget) => 0)
(enter-budget-depth! budget)
(check (budget-depth budget) => 1)
(count-budget-step! budget)
(check (budget-steps budget) => 1)
(count-budget-reference! budget)
(check (budget-references budget) => 1)

;; 3. Leaving the depth restores the previous level and never goes below zero.
(enter-budget-depth! budget)
(check (budget-depth budget) => 2)
(leave-budget-depth! budget)
(check (budget-depth budget) => 1)
(leave-budget-depth! budget)
(leave-budget-depth! budget)
(check (budget-depth budget) => 0)

;; 4. The step budget raises once the limit is reached.

(define step-budget (make-resource-budget 100 1 100))
(count-budget-step! step-budget)
(check (budget-steps step-budget) => 1)
(check-catch 'json-schema-budget-exceeded (count-budget-step! step-budget))
(check (budget-steps step-budget) => 2)

;; 5. The reference budget raises once the limit is reached.

(define ref-budget (make-resource-budget 100 100 2))
(count-budget-reference! ref-budget)
(count-budget-reference! ref-budget)
(check-catch 'json-schema-budget-exceeded (count-budget-reference! ref-budget))
(check (budget-references ref-budget) => 3)

;; 6. The depth budget raises once the limit is reached.

(define depth-budget (make-resource-budget 2 100 100))
(enter-budget-depth! depth-budget)
(enter-budget-depth! depth-budget)
(check-catch 'json-schema-budget-exceeded (enter-budget-depth! depth-budget))
(check (budget-depth depth-budget) => 3)

;; 7. Reset restores every counter while keeping the limits.
(reset-resource-budget! depth-budget)
(check (budget-depth depth-budget) => 0)
(check (budget-steps depth-budget) => 0)
(check (budget-references depth-budget) => 0)
(check (budget-max-depth depth-budget) => 2)
(check (budget-max-steps depth-budget) => 100)
(check (budget-max-references depth-budget) => 100)

;; 8. A reset budget accepts the full budget again.
(enter-budget-depth! depth-budget)
(check (budget-depth depth-budget) => 1)
(count-budget-step! depth-budget)
(check (budget-steps depth-budget) => 1)

;; 9. The budget error message mentions the budget.

(define (mentions-budget? text)
  (let ((probe "budget exceeded"))
    (let loop
      ((i 0))
      (cond
       ((> (+ i (string-length probe)) (string-length text)) #f)
       ((string=? (substring text i (+ i (string-length probe))) probe) #t)
       (else (loop (+ i 1)))
      ) ;cond
    ) ;let
  ) ;let
) ;define

(define caught-identifier
  (catch 'json-schema-budget-exceeded
    (lambda ()
      (let ((tiny (make-resource-budget 1 100 100)))
        (enter-budget-depth! tiny)
        (enter-budget-depth! tiny)
      ) ;let
    ) ;lambda
    (lambda args 'caught-by-identifier)
  ) ;catch
) ;define
(check caught-identifier => 'caught-by-identifier)
(check-true
  (mentions-budget? (guard (condition (#t condition))
                      (enter-budget-depth! (make-resource-budget 0 100 100))
                      "no error"
                    ) ;guard
  ) ;mentions-budget?
) ;check-true

(check-report)
