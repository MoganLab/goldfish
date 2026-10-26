(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi code core compaction fileops) (liii pi code core session-manager)
        (liii pi code core compaction))

(check-set-mode! 'report-failed)

(define (tc name path)
  (pi-tool-call-block (string-append "id-" name) name (list (cons "path" path)) #f)
) ;define

;; pi-extract-file-ops
(let* ((msgs (list (pi-assistant-message (list (tc "read" "/a")
                                               (tc "write" "/b")
                                               (tc "edit" "/c")
                                             ) ;list
                   ) ;pi-assistant-message
                   (pi-assistant-message (list (tc "read" "/d")
                                               (tc "read" "/a")
                                             ) ;list
                   ) ;pi-assistant-message
                 ) ;list
       ) ;msgs
       (ops (pi-extract-file-ops msgs))
      ) ;
  (check (cdr (assoc 'read ops)) => '("/a" "/d"))
  (check (cdr (assoc 'written ops)) => '("/b"))
  (check (cdr (assoc 'edited ops)) => '("/c"))
) ;let*

;; pi-compute-file-lists: 只读文件 = read - modified，且排序
(let* ((ops '((read . ("/z" "/b" "/a")) (written . ("/b")) (edited . ("/c"))))
       (lists (pi-compute-file-lists ops))
      ) ;
  (check (cdr (assoc 'read-files lists)) => '("/a" "/z"))
  (check (cdr (assoc 'modified-files lists)) => '("/b" "/c"))
) ;let*

;; pi-format-file-operations
(let ((text (pi-format-file-operations '("/a") '("/b"))))
  (check-true (string-contains? text "<read-files>\n/a\n</read-files>"))
  (check-true (string-contains? text "<modified-files>\n/b\n</modified-files>"))
) ;let

;; 空列表不产生段落
(check (pi-format-file-operations '() '()) => "")

(check-report)
