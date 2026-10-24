(import (liii check)
        (liii string)
        (liii pi code simplify format)
) ;import

(check-set-mode! 'report-failed)

(define (suggestion file line category message . maybe-suggestion)
  (append (list (cons 'file file)
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

;; pi-format-simplify-suggestions
;; 无建议时输出固定文案。
(check (pi-format-simplify-suggestions '())
  =>
  "No simplification opportunities found. Code looks clean."
) ;check

;; 标题含条数；条目按文件与行号排序。
(let* ((ss (list (suggestion "b.ts" 5 "clarity" "simplify b5" "try this")
                 (suggestion "a.ts" 20 "naming" "rename x" "use count")
                 (suggestion "a.ts" 3 "dead-code" "remove dead branch" "delete")
               ) ;list
       ) ;ss
       (out (pi-format-simplify-suggestions ss))
      ) ;
  (check-true (string-starts? out "## Simplify Suggestions (3)"))
  (define (prefix-len text marker)
    (string-length (car (string-split text marker)))
  ) ;define
  (let ((a3 (prefix-len out "a.ts:3"))
        (a20 (prefix-len out "a.ts:20"))
        (b5 (prefix-len out "b.ts:5"))
       ) ;
    (check-true (< a3 a20))
    (check-true (< a20 b5))
  ) ;let
  (check-true (string-contains? out "- [dead-code] a.ts:3: remove dead branch"))
  (check-true (string-contains? out "\n  Suggestion: delete"))
  (check-true (string-contains? out "- [naming] a.ts:20: rename x"))
  (check-true (string-contains? out "- [clarity] b.ts:5: simplify b5"))
) ;let*

;; 没有 suggestion 字段时不输出 Suggestion 行；没有行号时只输出文件
(let* ((ss (list (suggestion "m.scm" #f "clarity" "split it")))
       (out (pi-format-simplify-suggestions ss))
      ) ;
  (check-true (string-contains? out "- [clarity] m.scm: split it"))
  (check-false (string-contains? out "Suggestion:"))
) ;let*

;; 单条建议标题
(let* ((ss (list (suggestion "only.ts" 1 "naming" "x")))
       (out (pi-format-simplify-suggestions ss))
      ) ;
  (check-true (string-starts? out "## Simplify Suggestions (1)"))
) ;let*

(check-report)
