;; diverge-letrec: init-period forward reference (R7RS error vs s7 leniency).
(begin
  (display (letrec ((a b) (b 1)) a)) (newline)
  (display 'done))
