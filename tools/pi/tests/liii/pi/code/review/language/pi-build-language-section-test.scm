(import (liii check)
        (liii string)
        (liii pi code review language)
) ;import

(check-set-mode! 'report-failed)

;; pi-build-language-section
;; 生成与原生扩展等价的分语言检查清单段落。
(check (pi-build-language-section '()) => "")

(let ((s (pi-build-language-section '("typescript"))))
  (check-true (string-starts? s "\n\n### Typescript\n"))
  (check-true (string-contains? s "- Type safety:"))
  (check-false (string-contains? s "### Python")))

(let* ((langs '("typescript" "python" "typescript"))
       (s (pi-build-language-section langs)))
  (check-true (string-contains? s "### Typescript"))
  (check-true (string-contains? s "### Python"))
  ;; 去重：只出现一次
  (check (- (length (string-split s "### Typescript")) 1) => 1))

;; 未知语言不产生段落
(check (pi-build-language-section '("cobol")) => "")

(check-report)
