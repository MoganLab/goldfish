(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass project)
        (liii pi compass generate)
        (liii pi compass tour)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define codemap
  (pi-compass-generate-codemap fixture
    (pi-compass-detect-project fixture)
  ) ;pi-compass-generate-codemap
) ;define

;; pi-compass-detect-topics：顶层目录名 + testing / ci / database 指示词。
(check (pi-compass-detect-topics fixture codemap)
  =>
  '(".github" "spec" "src" "tests" "testing" "ci")
) ;check

;; Scheme 工程同样识别 testing（tests 目录）与 ci（.gitlab-ci.yml）。
(let* ((scheme-fixture "tests/fixtures/compass/scheme")
       (scheme-codemap (pi-compass-generate-codemap scheme-fixture
                         (pi-compass-detect-project scheme-fixture)
                       ) ;pi-compass-generate-codemap
       ) ;scheme-codemap
      ) ;
  (check (pi-compass-detect-topics scheme-fixture scheme-codemap)
    =>
    '("goldfish" "src" "tests" "tools" "testing" "ci")
  ) ;check
) ;let*

(check-report)
