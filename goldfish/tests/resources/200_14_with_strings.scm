;; 包含字符串内容的测试，忽略括号
(define (greeting name)
  (display (string-append "Hello, " name "!")
  (newline))

(let ((str "This string has (parens) in it ()()()"))
  (display str)
  (newline))

(define (test)
  (display "Unmatched parens in strings should be ignored: () ("))
  (newline))

(define x "mixed (content (like) this)")