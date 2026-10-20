(import (liii check)
        (liii string)
        (liii path)
        (liii hashlib)
        (liii pi compass project)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-detect-project
;; 非 git 目录回退到 cwd（native 的错误文案「必须在 git 仓库内」是假命题）；
;; id = sha256(remote || root)[0:12]。

(define dir (path-join (path-temp-dir) "gf-compass-project-test"))

(when (path-exists? dir)
  (path-rmdir dir)
) ;when
(path-mkdir dir :parents #t :exist-ok #t)

(dynamic-wind (lambda () #f)
  (lambda ()
    (let* ((abs (path->string (path-resolve dir)))
           (project (pi-compass-detect-project abs))
          ) ;
      (check (pi-compass-project-id project)
        =>
        (substring (sha256 abs) 0 12)
      ) ;check
      (check (pi-compass-project-name project) => "gf-compass-project-test")
      (check (pi-compass-project-root project) => abs)
      (check (pi-compass-project-remote project) => "")
      (check (string-length (pi-compass-project-id project)) => 12)
    ) ;let*
  ) ;lambda
  (lambda ()
    (when (path-exists? dir)
      (path-rmdir dir)
    ) ;when
  ) ;lambda
) ;dynamic-wind

(check (pi-compass-hash-id "abc") => (substring (sha256 "abc") 0 12))

(check-report)
