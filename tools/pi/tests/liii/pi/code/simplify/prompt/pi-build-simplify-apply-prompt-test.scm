(import (liii check)
        (liii string)
        (liii pi code simplify prompt)
) ;import

(check-set-mode! 'report-failed)

(define (file path status ranges available?)
  (list (cons 'path path)
    (cons 'status status)
    (cons 'ranges ranges)
    (cons 'ranges-available? available?)
  ) ;list
) ;define

;; pi-build-simplify-apply-prompt
;; 应用模式提示词：范围约束 + 逐文件修改 + 跑测试 + 总结并单列越界机会。

(let* ((files (list (file "src/calc.ts" "modified" '(((start . 21) (end . 33))) #t)
                    (file "src/util.ts" "added" #f #t)
                  ) ;list
       ) ;files
       (p (pi-build-simplify-apply-prompt files))
      ) ;
  (check-true (string-contains? p "**Preserve functionality**"))
  (check-true (string-contains? p "## Scope"))
  (check-true (string-contains? p "- src/calc.ts (modified; changed lines: 21-33)"))
  (check-true (string-contains? p "- src/util.ts (added; entire file is in scope)"))
  (check-true (string-contains? p "Apply changes one file at a time"))
  (check-true (string-contains? p "Run existing tests"))
  (check-true (string-contains? p "Summarize what you changed and why"))
  (check-true (string-contains? p "Outside scope"))
  (check-true (string-contains? p "Do NOT add new features"))
  (check-false (string-contains? p "\"suggestions\":"))
) ;let*

(check-report)
