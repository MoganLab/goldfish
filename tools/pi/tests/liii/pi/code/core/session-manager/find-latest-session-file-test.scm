(import (liii check)
  (liii path)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-latest-dir")))
       (_ (if (file-exists? tmp-dir) (path-rmdir tmp-dir) #t))
       (_ (path-mkdir tmp-dir))
       (f1 (path->string (path-join tmp-dir "2026-10-01T00-00-00-000Z_id1.jsonl")))
       (f2 (path->string (path-join tmp-dir "2026-10-02T00-00-00-000Z_id2.jsonl")))
       (f3 (path->string (path-join tmp-dir "2026-09-30T00-00-00-000Z_id0.jsonl"))))
  (save-session-header! f1 "id1" "2026-10-01T00:00:00.000Z" (path->string (path-temp-dir)))
  (save-session-header! f2 "id2" "2026-10-02T00:00:00.000Z" (path->string (path-temp-dir)))
  (save-session-header! f3 "id0" "2026-09-30T00:00:00.000Z" (path->string (path-temp-dir)))
  (let ((latest (find-latest-session-file tmp-dir)))
    (check latest => f2))
  (delete-file f1)
  (delete-file f2)
  (delete-file f3)
  (path-rmdir tmp-dir))

(check-report)
