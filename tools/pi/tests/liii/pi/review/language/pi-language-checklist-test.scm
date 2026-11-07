;; pi-language-checklist
;; 获取指定编程语言的代码审查关注点清单。
;;
;; 语法
;; ----
;; (pi-language-checklist lang)
;;
;; 参数
;; ----
;; lang : string?
;; 语言标识字符串。
;;
;; 返回值
;; ----
;; list?
;; 该语言专属的审查检查点描述字符串列表。

(import (liii check)
        (liii string)
        (liii pi review language)
) ;import

(check-set-mode! 'report-failed)

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
