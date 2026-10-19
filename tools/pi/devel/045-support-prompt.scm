;; [045] 验收点 3-4：Scope 措辞与提示词构造（由 045-support.scm 调用）。

(define (run-scope-and-prompt-checks!)
  (display "=== 验收点 3: Scope 描述构造 ===\n")
  (check
    (pi-build-scope-lines
      (list
        (simplify-file-entry "src/calc.ts"
          "modified"
          '(((start . 21) (end . 33)) ((start . 40) (end . 40)))
          #t
        ) ;simplify-file-entry
      ) ;list
    ) ;pi-build-scope-lines
    =>
    "- src/calc.ts (modified; changed lines: 21-33, 40)"
  ) ;check
  (check (pi-build-scope-lines (list (simplify-file-entry "src/util.ts" "added" #f #t)))
    =>
    "- src/util.ts (added; entire file is in scope)"
  ) ;check
  (check (pi-build-scope-lines (list (simplify-file-entry "src/old.ts" "modified" '() #t))
         ) ;pi-build-scope-lines
    =>
    "- src/old.ts (modified; deletions only — no current lines to simplify)"
  ) ;check
  (check (pi-build-scope-lines (list (simplify-file-entry "src/broken.ts" "modified" #f #f))
         ) ;pi-build-scope-lines
    =>
    "- src/broken.ts (modified; changed lines unavailable — inspect git diff before editing)"
  ) ;check

  (display "=== 验收点 4: 提示词构造 ===\n")
  (let* ((content (string-append "function f() {\n" "  let t = 0;\n"
                    "  t = t + 1;\n" "  return t;\n" "}\n\n" "far away line\n"
                  ) ;string-append
         ) ;content
         (files
           (list
             (simplify-file-entry "src/calc.ts"
               "modified"
               '(((start . 2) (end . 3)))
               #t
               content
             ) ;simplify-file-entry
             (simplify-file-entry "src/util.ts" "added" #f #t "export const a = 1;\n")
           ) ;list
         ) ;files
         (read-only (pi-build-simplify-prompt files))
         (apply-prompt (pi-build-simplify-apply-prompt files))
        ) ;
    (for-each (lambda (needle) (check-true (string-contains? read-only needle)))
      '("**Preserve functionality**" "**Apply project standards**"
        "**Enhance clarity**" "**Maintain balance**" "## Scope"
        "Only review and propose changes within the changed lines listed below."
        "- src/calc.ts (modified; changed lines: 2-3)"
        "- src/util.ts (added; entire file is in scope)" "## Process"
        "Do NOT add new features" "outside the listed line ranges"
        "do not call or request any tools" "\"suggestions\": [{\"file\":"
        "{\"suggestions\": []}" "let t = 0;" "### src/calc.ts (modified)")
    ) ;for-each
    (check-false (string-contains? read-only "far away line"))
    (for-each (lambda (needle) (check-true (string-contains? apply-prompt needle)))
      '("**Preserve functionality**" "## Scope"
        "- src/calc.ts (modified; changed lines: 2-3)"
        "Apply changes one file at a time" "Run existing tests"
        "Summarize what you changed and why" "Outside scope"
        "Do NOT add new features")
    ) ;for-each
    (check-false (string-contains? apply-prompt "\"suggestions\":"))
  ) ;let*

) ;define
