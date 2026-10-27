(import (liii check)
        (liii string)
        (liii pi review auto)
) ;import

(check-set-mode! 'report-failed)

(define (file-entry path lang)
  (list (cons 'path path) (cons 'language lang))
) ;define

;; pi-build-review-injection
;; 空编辑列表不注入。
(check (pi-build-review-injection '()) => #f)

(let* ((files (list (file-entry "src/a.ts" "typescript")
                    (file-entry "src/b.py" "python")
                  ) ;list
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check-true (string-starts? block "\n\n## Code Review\n\nYou edited: src/a.ts, src/b.py."))
  (check-true (string-contains? block "Before proceeding, briefly verify:"))
  (check-true (string-contains? block "### Typescript"))
  (check-true (string-contains? block "### Python"))
  (check-true (string-ends? block "Fix any issues found, then continue."))
) ;let*

;; 多文件同语言时只出现一次清单
(let* ((files (list (file-entry "a.ts" "typescript")
                    (file-entry "b.ts" "typescript")
                    (file-entry "c.scm" "scheme")
                  ) ;list
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check (- (length (string-split block "### Typescript")) 1) => 1)
  (check (- (length (string-split block "### Scheme")) 1) => 1)
  (check-true (string-contains? block "You edited: a.ts, b.ts, c.scm."))
) ;let*

(check-report)
