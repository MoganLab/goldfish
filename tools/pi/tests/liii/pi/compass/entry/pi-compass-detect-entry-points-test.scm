(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass entry)
        (liii pi compass packages)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define (entry-paths cwd)
  (map pi-compass-entry-point-path
    (pi-compass-detect-entry-points cwd (pi-compass-detect-packages cwd))
  ) ;map
) ;define

(define (entry-kind cwd path)
  (let ((ep (find (lambda (e)
                    (string=? (pi-compass-entry-point-path e) path)
                  ) ;lambda
              (pi-compass-detect-entry-points cwd (pi-compass-detect-packages cwd))
            ) ;find
       ) ;ep
      ) ;bindings
    (pi-compass-entry-point-kind ep)
  ) ;let
) ;define

;; pi-compass-detect-entry-points：npm main、常见入口、路由目录、配置文件。
(check (entry-paths fixture) => '("src/index.js" "src/routes" "src/api" "xmake.lua"))
(check (entry-kind fixture "src/index.js") => "main")
(check (entry-kind fixture "src/routes") => "route")
(check (entry-kind fixture "src/api") => "route")
(check (entry-kind fixture "xmake.lua") => "config")

;; Scheme 适配：liii/scheme/srfi/lib/libs/src 且直接含 .scm 的目录 → library。
(let ((scheme-fixture "tests/fixtures/compass/scheme"))
  (check (entry-paths scheme-fixture) => '("xmake.lua" "goldfish/liii" "src"))
  (check (entry-kind scheme-fixture "goldfish/liii") => "library")
  (check (entry-kind scheme-fixture "src") => "library")
) ;let

(check-report)
