(import (liii check)
        (liii string)
        (liii pi code review prompt)
) ;import

(check-set-mode! 'report-failed)

(define (file-entry path lang content)
  (list (cons 'path path) (cons 'language lang) (cons 'content content))
) ;define

;; pi-build-review-prompt
;; 含 JSON 结构化输出契约、严重级说明、空 findings 约定、分语言清单与文件列表。
(let* ((files (list (file-entry "src/app.ts" "typescript" "const x = 1;")
                    (file-entry "notes.txt" #f "hello")
                  ) ;list
       ) ;files
       (p (pi-build-review-prompt files))
      ) ;
  (check-true (string-contains? p "Return your findings as JSON"))
  (check-true (string-contains? p "do not call or request any tools"))
  (check-true (string-contains? p "\"findings\": [{\"severity\":"))
  (check-true (string-contains? p "CRITICAL: Security vulnerabilities"))
  (check-true (string-contains? p "HIGH: Bugs"))
  (check-true (string-contains? p "MEDIUM: Anti-patterns"))
  (check-true (string-contains? p "INFO: Style improvements"))
  (check-true (string-contains? p "Focus areas by language:"))
  (check-true (string-contains? p "### Typescript"))
  (check-true (string-contains? p "- Type safety:"))
  (check-true (string-contains? p "If no issues are found, return: `{\"findings\": []}`"))
  (check-true (string-contains? p "## Files to Review"))
  (check-true (string-contains? p "### src/app.ts (typescript)"))
  (check-true (string-contains? p "const x = 1;"))
  (check-true (string-contains? p "### notes.txt"))
  (check-false (string-contains? p "### notes.txt (typescript)"))
  (check-false (string-contains? p "### Python"))
) ;let*

;; 文件内容超过 4000 字符时截断
(let* ((long-content (make-string 5000 #\x))
       (files (list (file-entry "big.ts" "typescript" long-content)))
       (p (pi-build-review-prompt files))
      ) ;
  (check-true (string-contains? p "[truncated]"))
  (check (< (string-length p) 6000) => #t)
) ;let*

;; 空文件列表也能生成合法提示词（文件段为空）
(let ((p (pi-build-review-prompt '())))
  (check-true (string-contains? p "## Files to Review"))
  (check-false (string-contains? p "### ")))

;; pi-build-fallback-prompt
;; 非结构化回退提示词：文件列表 + 语言清单 + 只读审查指令。
(let* ((files (list (file-entry "src/app.py" "python" "print(1)")))
       (p (pi-build-fallback-prompt files))
      ) ;
  (check-true (string-contains? p "Review the following recently changed files"))
  (check-true (string-contains? p "- src/app.py"))
  (check-true (string-contains? p "### Python"))
  (check-true (string-contains? p "report your findings"))
) ;let*

(check-report)
