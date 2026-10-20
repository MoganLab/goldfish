(import (liii check)
        (liii string)
        (liii list)
        (liii pi compass conventions)
        (liii pi compass types)
) ;import

(check-set-mode! 'report-failed)

(define fixture "tests/fixtures/compass/project")

(define conventions (pi-compass-detect-conventions fixture))

(define (sources)
  (map pi-compass-convention-source conventions)
) ;define

;; pi-compass-detect-conventions
(check (sources) => '("AGENTS.md" "CLAUDE.md" "CONTRIBUTING.md"))

;; 内容截断到 2000 字符并追加提示。
(let ((agents (find (lambda (c)
                      (string=? (pi-compass-convention-source c) "AGENTS.md")
                    ) ;lambda
                conventions
              ) ;find
      ) ;agents
     ) ;
  (check (string-ends? (pi-compass-convention-content agents) "\n... (truncated)")
    =>
    #t
  ) ;check
  (check (> (string-length (pi-compass-convention-content agents)) 2000) => #t)
  (check (< (string-length (pi-compass-convention-content agents)) 2050) => #t)
) ;let

;; 短文件原样保留。
(let ((claude (find (lambda (c)
                      (string=? (pi-compass-convention-source c) "CLAUDE.md")
                    ) ;lambda
                conventions
              ) ;find
      ) ;claude
     ) ;
  (check (pi-compass-convention-content claude) => "# Claude instructions\n")
) ;let

;; 截断函数边界。
(check (pi-compass-truncate-convention "abcdef" 6) => "abcdef")
(check (pi-compass-truncate-convention "abcdefg" 6) => "abcdef\n... (truncated)")

(check-report)
