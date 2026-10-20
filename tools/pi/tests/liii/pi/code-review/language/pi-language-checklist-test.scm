(import (liii check)
        (liii string)
        (liii pi code-review language)
) ;import

(check-set-mode! 'report-failed)

;; pi-language-checklist
;; 每种受支持语言给出 5 条检查项，未知语言返回空列表。
(for-each
  (lambda (lang)
    (let ((items (pi-language-checklist lang)))
      (check (length items) => 5)
    ) ;let
  ) ;lambda
  '("typescript" "python" "go" "rust" "java" "php" "scheme")
) ;for-each

(let ((ts (pi-language-checklist "typescript")))
  (check-true (string-contains? (car ts) "Type safety"))
  (check-true (string-contains? (car (reverse ts)) "Security"))
) ;let

(let ((py (pi-language-checklist "python")))
  (check-true (string-contains? (car py) "Type hints"))
) ;let

(let ((scm (pi-language-checklist "scheme")))
  (check-true (string-contains? (car scm) "Data abstraction"))
  (check-true (string-contains? (car (reverse scm)) "Security"))
) ;let

(check (pi-language-checklist "cobol") => '())

(check-report)
