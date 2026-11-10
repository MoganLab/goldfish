;; Bad: Block comments hiding bracket issues
(define (test-block-comment-bad)
  #| This is a block comment |#
  (display "bracket issue hidden")
  (let ((x 42) #| unclosed here |#
  ;; bad - missing closing paren for let