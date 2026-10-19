;; [045] 验收点 5、7：建议解析降级与报告格式化（由 045-support.scm 调用）。

(define (run-parse-and-format-checks!)
  (display "=== 验收点 5: 建议解析与降级 ===\n")
  (let* ((text "```json\n{\"suggestions\":[{\"file\":\"b.py\",\"line\":3,\"category\":\"dead-code\",\"message\":\"unused\",\"suggestion\":\"delete\"}]}\n```"
         ) ;text
         (ss (pi-parse-simplify-suggestions text))
        ) ;
    (check (length ss) => 1)
    (check (%field 'file (car ss)) => "b.py")
    (check (%field 'line (car ss)) => 3)
    (check (%field 'suggestion (car ss)) => "delete")
  ) ;let*
  (check (pi-parse-simplify-suggestions "{\"suggestions\":[]}") => '())
  (let* ((text "{\"suggestions\":[{\"file\":\"x\",\"category\":\"\",\"message\":\"m\"},{\"file\":\"y.ts\",\"line\":2,\"category\":\"naming\",\"message\":\"ok\"}]}"
         ) ;text
         (ss (pi-parse-simplify-suggestions text))
        ) ;
    (check (length ss) => 1)
    (check (%field 'file (car ss)) => "y.ts")
  ) ;let*
  (let* ((ss (pi-parse-simplify-suggestions "not json at all")))
    (check (length ss) => 1)
    (check (%field 'category (car ss)) => "parse-error")
    (check-true (string-contains? (%field 'message (car ss))
                  "Could not parse structured simplification suggestions"
                ) ;string-contains?
    ) ;check-true
  ) ;let*
  (let ((ss (pi-parse-simplify-suggestions "{\"result\":\"ok\"}")))
    (check (%field 'category (car ss)) => "parse-error")
  ) ;let

  (display "=== 验收点 7: 报告格式化 ===\n")
  (check (pi-format-simplify-suggestions '())
    =>
    "No simplification opportunities found. Code looks clean."
  ) ;check
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
    (check-true (< (prefix-len out "a.ts:3") (prefix-len out "a.ts:20")))
    (check-true (< (prefix-len out "a.ts:20") (prefix-len out "b.ts:5")))
    (check-true (string-contains? out "- [dead-code] a.ts:3: remove dead branch"))
    (check-true (string-contains? out "\n  Suggestion: delete"))
  ) ;let*
  (let* ((out (pi-format-simplify-suggestions (list (suggestion "m.scm" #f "clarity" "split it"))
              ) ;pi-format-simplify-suggestions
         ) ;out
        ) ;
    (check-true (string-contains? out "- [clarity] m.scm: split it"))
    (check-false (string-contains? out "Suggestion:"))
  ) ;let*
) ;define
