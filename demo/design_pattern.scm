(import (liii check))

; Singleton
(define-object cat
  (define (@run)
    "I'm a running cat")
)

(check (cat :run) => "I'm a running cat")

; Factory
(define-class duck
  ((name string? "donald"))

  (define (%run)
    (string-append "Run as a duck (" name ")"))
  
  (define (@apply name)
    (let ((d (duck)))
      (d :set-name! name)
      d))
)

(define-class dog
  ((name string? "doggy"))

  (define (%run)
    (string-append "Run as a dog (" name ")"))
  
  (define (@apply name)
    (let ((d (dog)))
      (d :set-name! name)
      d))
)

(define-object animal
  (define (@create kind)
    (case kind
      ((duck)
       (duck "Duck"))
      ((dog)
       (dog "Dog"))
      (else (??? "No such kind"))))
)

(check (animal :create 'duck) => (duck "Duck"))
(check (animal :create 'dog) => (dog "Dog"))
(check ((animal :create 'duck) :run) => "Run as a duck (Duck)")
(check ((animal :create 'dog) :run) => "Run as a dog (Dog)")
