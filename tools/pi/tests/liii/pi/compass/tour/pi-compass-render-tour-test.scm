(import (liii check)
        (liii string)
        (liii pi compass tour)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-render-tour：标题、编号步骤、描述与结尾空行。
(define tour
  (pi-compass-tour "p1"
    "testing"
    "2026-10-21T00:00:00.000Z"
    (list (pi-compass-tour-step "tests/login.test.js" "Test file for login")
      (pi-compass-tour-step "spec/base.spec.js" "Test file for base")
    ) ;list
  ) ;pi-compass-tour
) ;define

(check (pi-compass-render-tour tour)
  =>
  "## Code Tour: testing\n\n**1.** `tests/login.test.js`\n   Test file for login\n\n**2.** `spec/base.spec.js`\n   Test file for base\n"
) ;check

;; 无匹配文件时的降级文案（退出码仍为 0）。
(check (pi-compass-render-tour
         (pi-compass-tour "p1" "nope" "2026-10-21T00:00:00.000Z" '())
       ) ;pi-compass-render-tour
  =>
  "No files found for topic \"nope\"."
) ;check

(check-report)
