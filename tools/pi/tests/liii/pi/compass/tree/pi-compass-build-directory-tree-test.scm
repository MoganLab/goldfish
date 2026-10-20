(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass tree)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define (names entries)
  (map pi-compass-entry-name entries)
) ;define


;; pi-compass-build-directory-tree
;; 深度 2、按名排序、跳过点文件（.github 例外）、目录名以独立 type 字段标注。
(check (names (pi-compass-build-directory-tree fixture))
  =>
  '(".github"
    "AGENTS.md"
    "CHANGELOG.md"
    "CLAUDE.md"
    "CONTRIBUTING.md"
    "Cargo.toml"
    "Dockerfile"
    "Jenkinsfile"
    "LICENSE"
    "Makefile"
    "README.md"
    "build.gradle"
    "composer.json"
    "docker-compose.yml"
    "go.mod"
    "package.json"
    "pom.xml"
    "pyproject.toml"
    "spec"
    "src"
    "tests"
    "xmake.lua")
) ;check

(let* ((tree (pi-compass-build-directory-tree fixture))
       (github (find (lambda (e) (string=? (pi-compass-entry-name e) ".github")) tree))
       (src (find (lambda (e) (string=? (pi-compass-entry-name e) "src")) tree))
       (ci (find (lambda (e) (string=? (pi-compass-entry-name e) "package.json")) tree))
      ) ;
  (check (pi-compass-entry-dir? github) => #t)
  (check (names (pi-compass-entry-children github)) => '("workflows"))
  (check (names (pi-compass-entry-children src))
    =>
    '("api"
      "auth"
      "config.js"
      "controllers"
      "index.js"
      "middleware"
      "models"
      "routes"
      "services"
      "utils")
  ) ;check
  (check (pi-compass-entry-dir? ci) => #f)
  (check (pi-compass-entry-children ci) => '())
) ;let*

;; 深度 1：只有根目录条目，目录不再带子项。
(check (names (pi-compass-build-directory-tree fixture 1))
  =>
  '(".github"
    "AGENTS.md"
    "CHANGELOG.md"
    "CLAUDE.md"
    "CONTRIBUTING.md"
    "Cargo.toml"
    "Dockerfile"
    "Jenkinsfile"
    "LICENSE"
    "Makefile"
    "README.md"
    "build.gradle"
    "composer.json"
    "docker-compose.yml"
    "go.mod"
    "package.json"
    "pom.xml"
    "pyproject.toml"
    "spec"
    "src"
    "tests"
    "xmake.lua")
) ;check

(check-report)
