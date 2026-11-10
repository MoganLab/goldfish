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

(import (scheme base) (liii check) (liii pi code core skills frontmatter))

(check-set-mode! 'report-failed)

(check (strip-frontmatter "---\nname: test\ndescription: desc\n---\nMain body content")
  =>
  "Main body content"
) ;check

;; 无闭合 --- 时原样返回
(check (strip-frontmatter "---\nname: unclosed") => "---\nname: unclosed")

;; 无 frontmatter 时原样返回
(check (strip-frontmatter "# plain") => "# plain")

(check-report)
