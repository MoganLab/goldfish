;; Good: Block comments correctly ignored during bracket matching
(define (test-block-comment-good)
  #| This is a multi-line block comment |#
  (display "Perfectly balanced")
  (let ((x 42) #| unm(matched here |# x))
  )  ; properly closed
;; good - the unmatched parentheses are inside comments, so file is balanced