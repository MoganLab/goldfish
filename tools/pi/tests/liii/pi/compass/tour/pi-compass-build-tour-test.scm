(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass project)
        (liii pi compass generate)
        (liii pi compass tour)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define codemap
  (pi-compass-generate-codemap fixture
    (pi-compass-detect-project fixture)
  ) ;pi-compass-generate-codemap
) ;define

(define (step-files tour)
  (map pi-compass-step-file (pi-compass-tour-steps tour))
) ;define

;; pi-compass-build-tour：主题目录下深度 ≤3 的源文件，最多 15 个。
(let* ((tour (pi-compass-build-tour fixture "src" codemap))
       (files (step-files tour))
      ) ;
  (check (pi-compass-tour-topic tour) => "src")
  (check (length files) => 10)
  (check (car files) => "src/api/routes.js")
  (check (pair? (member "src/index.js" files)) => #t)
  (check (pi-compass-step-description
           (find (lambda (s) (string=? (pi-compass-step-file s) "src/index.js"))
             (pi-compass-tour-steps tour)
           ) ;find
         ) ;pi-compass-step-description
    =>
    "Entry point for src module"
  ) ;check
) ;let*

;; testing 主题：收集测试目录（每目录最多 5 个）。
(let* ((tour (pi-compass-build-tour fixture "testing" codemap))
       (files (step-files tour))
      ) ;
  (check files => '("tests/login.test.js" "spec/base.spec.js"))
  (check (pi-compass-step-description (car (pi-compass-tour-steps tour)))
    =>
    "Test file for login"
  ) ;check
) ;let*

;; ci 主题：收集 .github/workflows 与 .gitlab-ci.yml / Jenkinsfile。
(let* ((tour (pi-compass-build-tour fixture "ci" codemap))
       (files (step-files tour))
      ) ;
  (check (pair? (member ".github/workflows/ci.yml" files)) => #t)
  (check (pair? (member ".gitlab-ci.yml" files)) => #t)
  (check (pair? (member "Jenkinsfile" files)) => #t)
) ;let*

;; 未知主题：空步骤（不报错）。
(check (step-files (pi-compass-build-tour fixture "no-such-topic" codemap))
  =>
  '()
) ;check

;; Scheme 源扩展名 .scm 可被 tour 收集。
(let* ((scheme-fixture "tests/fixtures/compass/scheme")
       (scheme-codemap (pi-compass-generate-codemap scheme-fixture
                         (pi-compass-detect-project scheme-fixture)
                       ) ;pi-compass-generate-codemap
       ) ;scheme-codemap
       (tour (pi-compass-build-tour scheme-fixture "goldfish" scheme-codemap))
      ) ;
  (check (step-files tour) => '("goldfish/liii/base.scm" "goldfish/liii/string.scm"))
) ;let*

(check-report)
