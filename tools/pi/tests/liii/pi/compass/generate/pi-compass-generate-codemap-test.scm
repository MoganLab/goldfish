(import (liii check)
        (liii string)
        (liii path)
        (liii pi compass project)
        (liii pi compass generate)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

;; pi-compass-content-hash
;; 16 位十六进制；同一工程两次调用稳定。
(let* ((hash1 (pi-compass-content-hash fixture))
       (hash2 (pi-compass-content-hash fixture))
      ) ;
  (check hash1 => hash2)
  (check (string-length hash1) => 16)
) ;let*

;; 深度 2 的改动必须让哈希变化（native 缺陷 2 的防回归）。
(let* ((before (pi-compass-content-hash fixture))
       (new-file (path->string (path-join (path fixture) "src" "brand-new.js")))
      ) ;
  (path-write-text (path new-file) "// new\n")
  (let ((after (pi-compass-content-hash fixture)))
    (check (not (string=? before after)) => #t)
  ) ;let
  (path-unlink (path new-file) #f)
  (check (pi-compass-content-hash fixture) => before)
) ;let*

;; pi-compass-generate-codemap
(let* ((project (pi-compass-detect-project fixture))
       (codemap (pi-compass-generate-codemap fixture project))
      ) ;
  (check (pi-compass-codemap? codemap) => #t)
  (check (pi-compass-codemap-project-id codemap)
    =>
    (pi-compass-project-id project)
  ) ;check
  (check (string? (pi-compass-codemap-project-name codemap)) => #t)
  (check (not (string-null? (pi-compass-codemap-project-name codemap))) => #t)
  (check (string-length (pi-compass-codemap-content-hash codemap)) => 16)
  (check (pair? (pi-compass-codemap-tree codemap)) => #t)
  (check (pair? (pi-compass-codemap-packages codemap)) => #t)
  (check (pair? (pi-compass-codemap-frameworks codemap)) => #t)
  (check (pair? (pi-compass-codemap-entry-points codemap)) => #t)
  (check (pair? (pi-compass-codemap-build-scripts codemap)) => #t)
  (check (pair? (pi-compass-codemap-conventions codemap)) => #t)
  (check (pair? (pi-compass-codemap-key-files codemap)) => #t)
) ;let*

;; Scheme 工程：无 manifest 时 packages/frameworks 自然为空，但 xmake 与库目录可识别。
(let* ((scheme-fixture "tests/fixtures/compass/scheme")
       (project (pi-compass-detect-project scheme-fixture))
       (codemap (pi-compass-generate-codemap scheme-fixture project))
      ) ;
  (check (pi-compass-codemap-packages codemap) => '())
  (check (pi-compass-codemap-frameworks codemap) => '())
  (check (pair? (member "xmake.lua" (map pi-compass-entry-point-path
                                     (pi-compass-codemap-entry-points codemap)
                                   ) ;map
              ) ;member
         ) ;pair?
    =>
    #t
  ) ;check
  (check (pair? (member "goldfish/liii" (map pi-compass-entry-point-path
                                        (pi-compass-codemap-entry-points codemap)
                                      ) ;map
              ) ;member
         ) ;pair?
    =>
    #t
  ) ;check
) ;let*

(check-report)
