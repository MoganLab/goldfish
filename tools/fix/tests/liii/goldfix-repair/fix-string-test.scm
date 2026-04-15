(import (liii check)
  (liii goldfix)
  (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; fix-string
;; 根据可信缩进修正缺失或多余的右括号。

(check (fix-string "(define x 1)")
  =>
  "(define x 1)"
) ;check

(check (fix-string "") => "")

(check (fix-string "(define x 1")
  =>
  "(define x 1)"
) ;check

(check (fix-string (&- #""
                      (define a 1
                      (define b 2)
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (define a 1)
             (define b 2)
             ""
  ) ;&-
) ;check

(check (fix-string (&- #""
                      (begin
                        (display "x"
                        (newline))
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (begin
               (display "x")
               (newline))
             ""
  ) ;&-
) ;check

(check (fix-string (&- #""
                      (begin
                        (display "x"
                      ) ;begin
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (begin
               (display "x")
             ) ;begin
             ""
  ) ;&-
) ;check

(check (fix-string "(define x 1))")
  =>
  "(define x 1)"
) ;check

(check (fix-string (&- #""
                      (begin
                        (display "x"))
                        (newline))
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (begin
               (display "x")
               (newline))
             ""
  ) ;&-
) ;check

(check (fix-string "(display \")\") ; )")
  =>
  "(display \")\") ; )"
) ;check

(check (fix-string "(&- #\"\"\n  (not-code)\n\"\")"
       ) ;fix-string
  =>
  "(&- #\"\"\n  (not-code)\n\"\")"
) ;check

(check (fix-string (&- #""
                      (define-library (liii base)
                        (begin
                          (define (foo x)
                            (+ x 1)
                      ;; trailing comment
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (define-library (liii base)
               (begin
                 (define (foo x)
                   (+ x 1))))
             ;; trailing comment
             ""
  ) ;&-
) ;check

(check (fix-string (&- #""
                      (if (or (>= 1 4) (not (char=? #\a #\())))
                          #f
                          #t
                      ) ;if
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (if (or (>= 1 4) (not (char=? #\a #\()))
                 #f
                 #t
             ) ;if
             ""
  ) ;&-
) ;check

(check-report)
