;; strip-frontmatter
;; 剥离 Markdown 文件顶部的 YAML frontmatter，仅返回正文。
;;
;; 语法
;; ----
;; (strip-frontmatter content) -> string
;;
;; 参数
;; ----
;; content : string
;; 文件完整内容。
;;
;; 返回值
;; ------
;; string
;; 剥离 frontmatter 后的正文。

(import (scheme base)
  (liii check)
  (liii pi code core skills frontmatter)
) ;import

(check-set-mode! 'report-failed)

(check (strip-frontmatter "---
name: test
description: desc
---
Main body content") => "Main body content")

;; 无闭合 --- 时原样返回
(check (strip-frontmatter "---
name: unclosed") => "---\nname: unclosed")

;; 无 frontmatter 时原样返回
(check (strip-frontmatter "# plain") => "# plain")

(check-report)
