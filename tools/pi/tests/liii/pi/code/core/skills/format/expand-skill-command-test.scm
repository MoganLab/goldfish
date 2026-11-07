;; expand-skill-command
;; 展开 /skill:<name> [args] 命令：读取 SKILL.md 剥离 frontmatter，封装为
;; 带 name/location 与相对路径基准声明的 <skill> 块并附加用户参数；技能
;; 未命中或非 /skill: 前缀时原样返回。
;;
;; 语法
;; ----
;; (expand-skill-command text skills) -> string
;;
;; 参数
;; ----
;; text : string
;; 用户提交的命令文本。
;; skills : list
;; 已加载的 <pi-skill> 列表。
;;
;; 返回值
;; ------
;; string
;; 展开后的复合文本；无法展开时原样返回。

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii tempfile)
  (liii shutil)
  (liii pi code core skills types)
  (liii pi code core skills format)
) ;import

(check-set-mode! 'report-failed)

(define tmp-dir
  (path->string (path-join (path-temp-dir) "gf-skills-expand-test"))
) ;define

(define skill-file (path-join tmp-dir "SKILL.md"))

(when (file-exists? tmp-dir)
  (rmtree tmp-dir)
) ;when

(path-mkdir tmp-dir :parents #t :exist-ok #t)
(path-write-text skill-file
  "---\nname: pdf-tools\ndescription: 提取 PDF\n---\n# PDF Instructions\nExtract text."
) ;path-write-text

(define skills
  (list (%make-pi-skill "pdf-tools" "提取 PDF" (path->string skill-file) tmp-dir #f)
  ) ;list
) ;define

;; 非 /skill: 前缀原样返回
(check (expand-skill-command "hello world" skills) => "hello world")

;; 未命中技能原样返回
(check (expand-skill-command "/skill:nope do" skills) => "/skill:nope do")

;; 命中展开：技能块 + 相对路径基准 + 正文 + 用户参数
(let ((expanded (expand-skill-command "/skill:pdf-tools do something" skills)))
  (check-true (string-starts? expanded "<skill name=\"pdf-tools\" location=\""))
  (check-true (string-contains? expanded
                (string-append "References are relative to " tmp-dir ".")
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? expanded "# PDF Instructions"))
  (check-true (string-contains? expanded "</skill>"))
  (check-true (string-ends? expanded "</skill>\n\ndo something"))
) ;let

;; 无参数时不追加参数尾巴
(let ((no-args (expand-skill-command "/skill:pdf-tools" skills)))
  (check-true (string-ends? no-args "</skill>"))
) ;let

(rmtree tmp-dir)

(check-report)
