(import (liii check)
  (liii pi ai api anthropic-messages)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

(let ((decls (anthropic-build-tool-declarations-from-names '("bash" "read")
               get-tool-definition
             ) ;anthropic-build-tool-declarations-from-names
      ) ;decls
     ) ;
  (check (length decls) => 2)
  (check (cdr (assoc "name" (car decls))) => "bash")
  (check (cdr (assoc "name" (cadr decls))) => "read")
) ;let

(check-report)
