(import (liii check)
        (liii goldfix)
        (liii raw-string))

(check-set-mode! 'report-failed)

;; fix-string
;; 根据可信缩进修正缺失或多余的右括号。

(check (fix-string "(define x 1)")
       => "(define x 1)")

(check (fix-string "")
       => "")

(check (fix-string "(define x 1")
       => "(define x 1)")

(check (fix-string (&- #""
                      (define a 1
                      (define b 2)
                      ""
               ) ;&-
       )
       => (&- #""
             (define a 1)
             (define b 2)
             ""
      ) ;&-
)

(check (fix-string (&- #""
                      (begin
                        (display "x"
                        (newline))
                      ""
               ) ;&-
       )
       => (&- #""
             (begin
               (display "x")
               (newline))
             ""
      ) ;&-
)

(check (fix-string (&- #""
                      (begin
                        (display "x"
                      ) ;begin
                      ""
               ) ;&-
       )
       => (&- #""
             (begin
               (display "x")
             ) ;begin
             ""
      ) ;&-
)

(check (fix-string "(define x 1))")
       => "(define x 1)")

(check (fix-string (&- #""
                      (begin
                        (display "x"))
                        (newline))
                      ""
               ) ;&-
       )
       => (&- #""
             (begin
               (display "x")
               (newline))
             ""
      ) ;&-
)

(check (fix-string "(display \")\") ; )")
       => "(display \")\") ; )")

(check (fix-string "(&- #\"\"\n  (not-code)\n\"\")")
       => "(&- #\"\"\n  (not-code)\n\"\")")

(check (fix-string (&- #""
                      (define-library (liii base)
                        (begin
                          (define (foo x)
                            (+ x 1)
                      ;; trailing comment
                      ""
               ) ;&-
       )
       => (&- #""
             (define-library (liii base)
               (begin
                 (define (foo x)
                   (+ x 1))))
             ;; trailing comment
             ""
      ) ;&-
)

(check (fix-string (&- #""
                      (if (or (>= 1 4) (not (char=? #\a #\())))
                          #f
                          #t
                      ) ;if
                      ""
               ) ;&-
       )
       => (&- #""
             (if (or (>= 1 4) (not (char=? #\a #\()))
                 #f
                 #t
             ) ;if
             ""
      ) ;&-
)

(check-report)
