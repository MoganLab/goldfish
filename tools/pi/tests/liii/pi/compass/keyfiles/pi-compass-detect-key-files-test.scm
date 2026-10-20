(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass keyfiles)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define key-files (pi-compass-detect-key-files "tests/fixtures/compass/project"))

;; pi-compass-detect-key-files：按清单顺序返回实际存在的文件。
(check (map pi-compass-key-file-path key-files)
  =>
  '("README.md"
    "LICENSE"
    "CHANGELOG.md"
    "CONTRIBUTING.md"
    "AGENTS.md"
    "CLAUDE.md"
    "Dockerfile"
    "docker-compose.yml"
    "Makefile"
    ".github/workflows"
    ".gitlab-ci.yml"
    "Jenkinsfile")
) ;check

(check (pi-compass-key-file-description
         (find (lambda (k) (string=? (pi-compass-key-file-path k) "README.md"))
           key-files
         ) ;find
       ) ;pi-compass-key-file-description
  =>
  "Project documentation"
) ;check

(check (map pi-compass-key-file-path
         (pi-compass-detect-key-files "tests/fixtures/compass/scheme")
       ) ;map
  =>
  '("README.md" "Dockerfile" ".gitlab-ci.yml")
) ;check

(check-report)
