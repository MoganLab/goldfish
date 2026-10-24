(import (liii check)
        (liii pi review files)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-changed-files
;; 解析 git diff --name-status 输出。
(check (pi-parse-changed-files "") => '())

(let* ((out (string-append "M\tsrc/app.ts\n"
             "A\tsrc/new.py\n"
             "R100\told/name.ts\tnew/name.ts\n"
             "C75\tsrc/a.ts\tsrc/b.ts\n"
             "D\tremoved.ts\n"
           ) ;string-append
       ) ;out
       (files (pi-parse-changed-files out))
      ) ;
  (check (length files) => 4)
  (check (cdr (assoc 'path (list-ref files 0))) => "src/app.ts")
  (check (cdr (assoc 'status (list-ref files 0))) => "modified")
  (check (cdr (assoc 'path (list-ref files 1))) => "src/new.py")
  (check (cdr (assoc 'status (list-ref files 1))) => "added")
  (check (cdr (assoc 'path (list-ref files 2))) => "new/name.ts")
  (check (cdr (assoc 'status (list-ref files 2))) => "renamed")
  (check (cdr (assoc 'path (list-ref files 3))) => "src/b.ts")
  (check (cdr (assoc 'status (list-ref files 3))) => "copied")
) ;let*

;; 空行与未知状态忽略
(let ((files (pi-parse-changed-files "\n\nX\tweird.ts\n\nM\ta.ts\n")))
  (check (length files) => 1)
  (check (cdr (assoc 'path (car files))) => "a.ts"))

(check-report)
