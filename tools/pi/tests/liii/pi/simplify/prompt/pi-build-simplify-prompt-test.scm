(import (liii check)
        (liii string)
        (liii pi simplify prompt)
) ;import

(check-set-mode! 'report-failed)

(define (file path status ranges available? content)
  (list (cons 'path path)
    (cons 'status status)
    (cons 'ranges ranges)
    (cons 'ranges-available? available?)
    (cons 'content content)
  ) ;list
) ;define

(define sample-content
  (string-append "function f() {\n"
    "  let t = 0;\n"
    "  t = t + 1;\n"
    "  return t;\n"
    "}\n"
    "\n"
    "line 20 is far away\n"
  ) ;string-append
) ;define

;; pi-build-simplify-prompt
;; 只读模式提示词：四原则 + Scope + Process + JSON 契约 + 改动行上下文。

(let* ((files (list (file "src/calc.ts" "modified"
                          '(((start . 2) (end . 3)))
                          #t
                          sample-content
                        ) ;list
                    (file "src/util.ts" "added" #f #t "export const a = 1;\n")
                  ) ;list
       ) ;files
       (p (pi-build-simplify-prompt files))
      ) ;
  ;; 四原则
  (check-true (string-contains? p "**Preserve functionality**"))
  (check-true (string-contains? p "**Apply project standards**"))
  (check-true (string-contains? p "**Enhance clarity**"))
  (check-true (string-contains? p "**Maintain balance**"))
  ;; Scope / Process
  (check-true (string-contains? p "## Scope"))
  (check-true (string-contains? p "Only review and propose changes within the changed lines listed below."))
  (check-true (string-contains? p "- src/calc.ts (modified; changed lines: 2-3)"))
  (check-true (string-contains? p "- src/util.ts (added; entire file is in scope)"))
  (check-true (string-contains? p "## Process"))
  (check-true (string-contains? p "1. Read each changed file"))
  (check-true (string-contains? p "5. Summarize the opportunities"))
  ;; 禁止项
  (check-true (string-contains? p "Do NOT add new features"))
  (check-true (string-contains? p "change public APIs"))
  (check-true (string-contains? p "outside the listed line ranges"))
  ;; 只读与 JSON 契约
  (check-true (string-contains? p "do not call or request any tools"))
  (check-true (string-contains? p "\"suggestions\": [{\"file\":"))
  (check-true (string-contains? p "\"category\":"))
  (check-true (string-contains? p "\"suggestion\":"))
  (check-true (string-contains? p "{\"suggestions\": []}"))
  ;; 改动行上下文内联：包含改动行与邻近行，不包含远处行
  (check-true (string-contains? p "let t = 0;"))
  (check-true (string-contains? p "t = t + 1;"))
  (check-false (string-contains? p "line 20 is far away"))
  (check-true (string-contains? p "### src/calc.ts (modified)"))
  (check-true (string-contains? p "### src/util.ts (added)"))
) ;let*

;; 行范围不可用时内联整文件内容（截断标记）
(let* ((content (make-string 5000 #\x))
       (files (list (file "big.ts" "modified" #f #f content)))
       (p (pi-build-simplify-prompt files))
      ) ;
  (check-true (string-contains? p "changed lines unavailable"))
  (check-true (string-contains? p "[truncated]"))
  (check (< (string-length p) 7000) => #t)
) ;let*

;; 纯删除文件没有可内联的当前行
(let* ((files (list (file "old.ts" "modified" '() #t "should not appear")))
       (p (pi-build-simplify-prompt files))
      ) ;
  (check-true (string-contains? p "deletions only"))
  (check-false (string-contains? p "should not appear"))
) ;let*

;; 空文件列表也能生成合法提示词
(let ((p (pi-build-simplify-prompt '())))
  (check-true (string-contains? p "## Scope"))
  (check-true (string-contains? p "## Process"))
) ;let

(check-report)
