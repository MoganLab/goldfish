;; diverge-unspecified: single unspecified through values (R7RS vs s7 fold).
(begin
  (display (call-with-values (lambda () (values (if #f #f))) list)) (newline)
  (display 'done))
