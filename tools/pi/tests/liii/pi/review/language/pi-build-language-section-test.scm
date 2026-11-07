;; pi-build-language-section
;; 汇总指定语言集合的代码审查清单段落文本。
;;
;; 语法
;; ----
;; (pi-build-language-section langs)
;;
;; 参数
;; ----
;; langs : list?
;; 语言标识字符串列表。
;;
;; 返回值
;; ----
;; string?
;; 拼接去重后的分语言检查清单 Markdown 文本。

(import (liii check)
        (liii string)
        (liii pi review language)
) ;import

(check-set-mode! 'report-failed)

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
