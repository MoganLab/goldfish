(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction fileops)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(define (tc name path)
  (pi-tool-call-block (string-append "id-" name)
    name
    (list (cons "path" path))
    #f
  ) ;pi-tool-call-block
) ;define

(let* ((msgs
         (list (pi-assistant-message (list (tc "read" "/a") (tc "write" "/b") (tc "edit" "/c"))
               ) ;pi-assistant-message
           (pi-assistant-message (list (tc "read" "/d") (tc "read" "/a")))
         ) ;list
       ) ;msgs
       (ops (pi-extract-file-ops msgs))
      ) ;
  (check (cdr (assoc 'read ops)) => '("/a" "/d"))
  (check (cdr (assoc 'written ops)) => '("/b"))
  (check (cdr (assoc 'edited ops)) => '("/c"))
) ;let*

(check-report)
