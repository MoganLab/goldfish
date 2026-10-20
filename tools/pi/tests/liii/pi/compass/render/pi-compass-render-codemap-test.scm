(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass project)
        (liii pi compass generate)
        (liii pi compass render)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define codemap
  (pi-compass-generate-codemap fixture
    (pi-compass-detect-project fixture)
  ) ;pi-compass-generate-codemap
) ;define

(define markdown (pi-compass-render-codemap codemap))

;; pi-compass-render-codemap：章节齐全。
(for-each
  (lambda (section)
    (check (string-contains markdown section) => #t)
  ) ;lambda
  '("## Codebase Map:"
    "### Directory Structure"
    "### Packages"
    "### Frameworks"
    "### Entry Points"
    "### Build / Test / Deploy"
    "### Conventions"
    "### Key Files")
) ;for-each

;; 章节顺序固定：Directory Structure → Packages；Conventions → Key Files。
(check (< (string-position "### Directory Structure" markdown)
       (string-position "### Packages" markdown)
     ) ;<
  =>
  #t
) ;check
(check (< (string-position "### Conventions" markdown)
       (string-position "### Key Files" markdown)
     ) ;<
  =>
  #t
) ;check

;; 条目形态。
(check (string-contains markdown "```\n├── .github/") => #t)
(check (string-contains markdown "- **compass-sandbox** v0.1.0 (npm)") => #t)
(check (string-contains markdown "-- Project documentation") => #t)

;; 空章节省略：Scheme 工程无 Packages / Frameworks。
(let* ((scheme-fixture "tests/fixtures/compass/scheme")
       (scheme-codemap (pi-compass-generate-codemap scheme-fixture
                         (pi-compass-detect-project scheme-fixture)
                       ) ;pi-compass-generate-codemap
       ) ;scheme-codemap
       (scheme-md (pi-compass-render-codemap scheme-codemap))
      ) ;
  (check (string-contains scheme-md "### Packages") => #f)
  (check (string-contains scheme-md "### Frameworks") => #f)
  (check (string-contains scheme-md "### Entry Points") => #t)
  (check (string-contains scheme-md "### Build / Test / Deploy") => #t)
) ;let*

(check-report)
