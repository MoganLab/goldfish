(import (liii check)
        (liii string)
        (liii pi modes flow)
) ;import

(check-set-mode! 'report-failed)

;; pi-auto-review-system-prompt
;; 返回一个系统提示词提供者：无 write/edit 时保持基础提示词，
;; 有 write/edit 时追加 ## Code Review 注入块。
(define tool-call
  (lambda (name path)
    (list (cons "type" "toolCall")
      (cons "name" name)
      (cons "arguments" (list (cons "path" path)))
    ) ;list
  ) ;lambda
) ;define

(let ((provider (pi-auto-review-system-prompt "BASE")))
  (check (provider '()) => "BASE")
  (check (provider (list (tool-call "read" "a.ts"))) => "BASE")
  (let ((injected (provider (list (tool-call "write" "a.ts")))))
    (check-true (string-starts? injected "BASE\n\n## Code Review"))
    (check-true (string-contains? injected "You edited: a.ts."))
    (check-true (string-contains? injected "### Typescript"))
    (check-true (string-ends? injected "Fix any issues found, then continue."))
  ) ;let
) ;let

(check-report)
