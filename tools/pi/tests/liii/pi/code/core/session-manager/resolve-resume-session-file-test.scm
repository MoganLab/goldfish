(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; resolve-resume-session-file：解析恢复来源（session-file / continue 优先级）
;; 显式 session-file 优先，其次 --continue 取目录最新会话，否则返回 #f

(let* ((sdir (path->string (path-join (path-temp-dir) "test-resolve-resume")))
       (older (path->string (path-join sdir "2026-10-01T00-00-00-000Z_older.jsonl")))
       (latest (path->string (path-join sdir "2026-10-02T00-00-00-000Z_latest.jsonl"))))
  (when (file-exists? sdir)
    (rmtree sdir))
  (path-mkdir sdir :parents #t :exist-ok #t)
  (save-session-header! older "older" "2026-10-01T00:00:00.000Z" "/work")
  (save-session-header! latest "latest" "2026-10-02T00:00:00.000Z" "/work")
  ;; session-file 优先于目录最新文件
  (check (resolve-resume-session-file `((session-file . ,older) (continue . #t)) sdir)
    => older)
  ;; --continue 取目录内最新
  (check (resolve-resume-session-file '((continue . #t)) sdir) => latest)
  ;; 无恢复来源返回 #f
  (check-false (resolve-resume-session-file '() sdir))
  (rmtree sdir))

;; 目录不存在时 --continue 也返回 #f
(check-false
  (resolve-resume-session-file '((continue . #t))
    (path->string (path-join (path-temp-dir) "test-resolve-resume-missing"))))

(check-report)
