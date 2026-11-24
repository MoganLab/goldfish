;; markdown-extensions
;; Markdown 格式化处理器接管的文件后缀。
;;
;; 语法
;; ----
;; markdown-extensions
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; list of string，包含 .md 和 .markdown。

(import (scheme base) (liii check) (liii goldfmt lang) (liii goldfmt markdown))

(check-set-mode! 'report-failed)

(check markdown-extensions => '(".md" ".markdown"))
(check-true (not (not (lang-for-extension ".md"))))

(check-report)
