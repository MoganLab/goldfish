(import (liii check)
        (liii string)
        (liii list)
        (liii path)
        (liii pi compass packages)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define pkgs (pi-compass-detect-packages fixture))

(define (by-manager manager)
  (find (lambda (p) (string=? (pi-compass-package-manager p) manager)) pkgs)
) ;define

;; pi-compass-detect-packages：npm / go / cargo / poetry / composer / gradle / maven。
(check (map pi-compass-package-manager pkgs)
  =>
  '("npm" "go" "cargo" "poetry" "composer" "gradle" "maven")
) ;check

(let ((npm (by-manager "npm")))
  (check (pi-compass-package-name npm) => "compass-sandbox")
  (check (pi-compass-package-version npm) => "0.1.0")
  (check (pi-compass-package-dependencies npm) => '("express" "react" "vitest"))
) ;let

(let ((go (by-manager "go")))
  (check (pi-compass-package-name go) => "github.com/example/compass")
  (check (pi-compass-package-version go) => #f)
  (check (pi-compass-package-dependencies go)
    =>
    '("github.com/gin-gonic/gin" "github.com/stretchr/testify")
  ) ;check
) ;let

(let ((cargo (by-manager "cargo")))
  (check (pi-compass-package-name cargo) => "compass-rs")
  (check (pi-compass-package-version cargo) => "0.2.0")
  (check (pi-compass-package-dependencies cargo) => '("tokio" "serde" "axum" "clap"))
) ;let

(let ((py (by-manager "poetry")))
  (check (pi-compass-package-name py) => "compass-py")
  (check (pi-compass-package-version py) => "1.2.3")
  (check (pi-compass-package-dependencies py) => '("fastapi" "pytest"))
) ;let

(check (pi-compass-package-dependencies (by-manager "composer"))
  =>
  '("laravel/framework")
) ;check

(check (pi-compass-package-name (by-manager "gradle")) => "")
(check (pi-compass-package-dependencies (by-manager "maven")) => '())

;; pyproject.toml 无 [tool.poetry] → pip。
(let* ((dir (path-join (path-temp-dir) "gf-compass-pip-test"))
       (file (path->string (path-join dir "pyproject.toml")))
      ) ;
  (when (path-file? (path file)) (path-unlink (path file) #f))
  (when (path-dir? dir) (path-rmdir dir))
  (path-mkdir dir :parents #t :exist-ok #t)
  (path-write-text (path file)
    "[project]\nname = \"demo-pip\"\nversion = \"0.1.0\"\ndependencies = [\"requests>=2.0\"]\n"
  ) ;path-write-text
  (let ((pkgs (pi-compass-detect-packages (path->string dir))))
    (check (pi-compass-package-manager (car pkgs)) => "pip")
    (check (pi-compass-package-name (car pkgs)) => "demo-pip")
    (check (pi-compass-package-dependencies (car pkgs)) => '("requests"))
  ) ;let
  (path-unlink (path file) #f)
  (path-rmdir dir)
) ;let*

;; 空目录不产生包记录。
(check (pi-compass-detect-packages "tests/fixtures/compass/scheme") => '())

(check-report)
