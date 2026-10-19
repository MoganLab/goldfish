(import (liii check)
        (liii string)
        (liii pi code-review format)
) ;import

(check-set-mode! 'report-failed)

(define (finding severity file line category message . maybe-suggestion)
  (append (list (cons 'severity severity)
            (cons 'file file)
            (cons 'line line)
            (cons 'category category)
            (cons 'message message)
          ) ;list
    (if (pair? maybe-suggestion)
      (list (cons 'suggestion (car maybe-suggestion)))
      '()
    ) ;if
  ) ;append
) ;define

;; pi-format-review-findings
;; 无问题时输出固定文案。
(check (pi-format-review-findings '()) => "Code review complete: no issues found. Code looks clean."
) ;check

;; 标题含各严重级计数，条目按严重级降序
(let* ((fs (list (finding "INFO" "c.ts" 3 "style" "minor")
                 (finding "HIGH" "b.ts" 2 "bug" "oops" "wrap it")
                 (finding "CRITICAL" "a.ts" 1 "security" "bad" "fix now")
                 (finding "HIGH" "d.ts" 4 "type-safety" "any")
               ) ;list
       ) ;fs
       (out (pi-format-review-findings fs))
      ) ;
  (check-true (string-starts? out "## Code Review Findings (1 CRITICAL, 2 HIGH, 1 INFO)"))
  ;; 按严重级降序：用分割前缀长度比较首次出现位置
  (define (prefix-len text marker)
    (string-length (car (string-split text marker)))
  ) ;define
  (let ((critical-pos (prefix-len out "**CRITICAL**"))
        (high-pos (prefix-len out "**HIGH**"))
        (info-pos (prefix-len out "**INFO**"))
       ) ;
    (check-true (< critical-pos high-pos))
    (check-true (< high-pos info-pos))
  ) ;let
  (check-true (string-contains? out "- **CRITICAL** [security] a.ts:1: bad"))
  (check-true (string-contains? out "\n  Suggestion: fix now"))
  (check-true (string-contains? out "- **HIGH** [bug] b.ts:2: oops"))
  (check-true (string-contains? out "- **INFO** [style] c.ts:3: minor"))
) ;let*

;; 无行号时只输出 file
(let* ((fs (list (finding "MEDIUM" "x.scm" #f "naming" "rename")))
       (out (pi-format-review-findings fs))
      ) ;
  (check-true (string-contains? out "- **MEDIUM** [naming] x.scm: rename"))
) ;let*

;; 单一级别
(let* ((fs (list (finding "HIGH" "a.ts" 1 "bug" "x") (finding "HIGH" "b.ts" 2 "bug" "y")))
       (out (pi-format-review-findings fs))
      ) ;
  (check-true (string-starts? out "## Code Review Findings (2 HIGH)"))
) ;let*

(check-report)
