;; fix-string
;; 根据可信缩进修正缺失或多余的右括号，只返回修正后的源码。
;;
;; 语法
;; ----
;; (fix-string source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; string? 修正后的源码文本

(import (liii check) (liii goldfix) (liii raw-string))

(check-set-mode! 'report-failed)

(check (fix-string "(define x 1)") => "(define x 1)")

(check (fix-string "") => "")

(check (fix-string "(define x 1") => "(define x 1)")

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

(check (fix-string "(define x 1))") => "(define x 1)")

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

(check (fix-string "(display \")\") ; )") => "(display \")\") ; )")

(check (fix-string "(&- #\"\"\n  (not-code)\n\"\")")
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

;; 独立的 | 标识符不应被当作 bar symbol 的开始定界符。

(check (fix-string (&- #""
                      (define-regexp-grammar
                        (:menu-item (:or ---
                                      |
                                      (group :%1)
                                      (text :%1)
                                    ) ;:or
                        ) ;:menu-item
                      ) ;define-regexp-grammar
                      ""
                   ) ;&-
       ) ;fix-string
  =>
  (&- #""
             (define-regexp-grammar
               (:menu-item (:or ---
                             |
                             (group :%1)
                             (text :%1)
                           ) ;:or
               ) ;:menu-item
             ) ;define-regexp-grammar
             ""
  ) ;&-
) ;check

(check-report)
