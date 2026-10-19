(import (liii check)
        (liii string)
        (liii pi simplify ranges)
) ;import

(check-set-mode! 'report-failed)

;; pi-diff-ranges-command
;; 逐文件获取改动行范围；staged 使用 --cached，回退 ref 时忽略 --staged。

(check (pi-diff-ranges-command "HEAD" #f "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff HEAD -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "HEAD" #t "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff --cached -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "HEAD~1" #t "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff HEAD~1 -- src/a.ts"
) ;check

(check (pi-diff-ranges-command "main" #f "src/a.ts")
  =>
  "git diff --unified=0 --no-ext-diff main -- src/a.ts"
) ;check

;; 不安全的 ref / path 使用单引号包裹
(check-true (string-contains? (pi-diff-ranges-command "main; rm -rf /tmp/x" #f "a b.ts")
                "'main; rm -rf /tmp/x'"
              ) ;string-contains?
) ;check-true
(check-true (string-contains? (pi-diff-ranges-command "HEAD" #f "a b.ts")
                "'a b.ts'"
              ) ;string-contains?
) ;check-true

(check-report)
