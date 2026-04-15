(import (liii check)
        (liii goldfix-edit)
        (liii goldfix-record))

(check-set-mode! 'report-failed)

;; apply-edits
;; 将 insert/delete edit 应用于原始 source。

(check (apply-edits "(define x 1"
                    (list (make-fix-edit :kind 'insert
                                         :offset 11
                                         :text ")"
                                         :reason "eof"
                                         :open-offset 0)))
       => "(define x 1)")

(check (apply-edits "(define x 1))"
                    (list (make-fix-edit :kind 'delete
                                         :start 12
                                         :end 13
                                         :reason "extra-close"
                                         :open-offset #f)))
       => "(define x 1)")

(check (apply-edits "(begin\n  (display \"x\"))\n  (newline))"
                    (list (make-fix-edit :kind 'delete
                                         :start 22
                                         :end 23
                                         :reason "premature-close"
                                         :open-offset 0)))
       => "(begin\n  (display \"x\")\n  (newline))")

;; offset 从后往前应用，前面的 edit 不会影响后面的原始 offset。

(check (apply-edits "(a b))"
                    (list (make-fix-edit :kind 'delete
                                         :start 5
                                         :end 6
                                         :reason "extra-close"
                                         :open-offset #f)
                          (make-fix-edit :kind 'insert
                                         :offset 0
                                         :text "'"
                                         :reason "test"
                                         :open-offset #f)))
       => "'(a b)")

(check-report)
