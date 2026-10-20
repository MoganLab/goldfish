(import (liii check)
        (liii string)
        (liii pi compass prompt)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define (make-codemap conventions)
  (pi-compass-codemap "p1"
    "demo"
    "2026-10-21T00:00:00.000Z"
    "0123456789abcdef"
    '()
    '()
    '()
    '()
    '()
    conventions
    '()
  ) ;pi-compass-codemap
) ;define

;; 未超限：只加两个换行的前缀，不截断。
(let ((block (pi-compass-build-injection
               (make-codemap
                 (list (pi-compass-convention "AGENTS.md" "# Rules\n"))
               ) ;make-codemap
               6000
             ) ;pi-compass-build-injection
      ) ;block
     ) ;
  (check (string-starts? block "\n\n## Codebase Map: demo") => #t)
  (check (string-ends? block "# Rules\n") => #t)
  (check (string-contains block "codemap truncated") => #f)
) ;let

;; 超限：截断到 6000 字符并追加脚本模式提示。
(let* ((long-content (make-string 8000 #\x))
       (block (pi-compass-build-injection
                (make-codemap
                  (list (pi-compass-convention "AGENTS.md" long-content))
                ) ;make-codemap
                6000
              ) ;pi-compass-build-injection
       ) ;block
      ) ;
  (check (string-ends? block
           "... (codemap truncated, run --codemap to see full output)"
         ) ;string-ends?
    =>
    #t
  ) ;check
  (check (< (string-length block) 6100) => #t)
) ;let*

(check-report)
