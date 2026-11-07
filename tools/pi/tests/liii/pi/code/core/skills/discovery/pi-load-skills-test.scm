;; pi-load-skills
;; 技能加载器：显式 --skill 路径与默认目录（gf 配置目录 skills 优先、
;; pi agent 目录 skills 兼容、当前目录 .pi/skills）多源合并扫描；
;; SKILL.md 边界停止递归、跳过隐藏目录与 node_modules、未写 name 时回退
;; 父目录名、同名冲突首见胜出（winner/loser）。
;;
;; 语法
;; ----
;; (pi-load-skills :skill-paths paths :cwd cwd :include-defaults bool) -> list
;;
;; 参数
;; ----
;; skill-paths : list
;; 显式技能路径（目录或 SKILL.md 文件）。
;; cwd : string
;; 项目级目录查找基准。
;; include-defaults : boolean
;; 是否合并默认目录。
;;
;; 返回值
;; ------
;; list
;; 去重后的 <pi-skill> 列表。

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii tempfile)
  (liii shutil)
  (liii pi code core skills types)
  (liii pi code core skills discovery)
) ;import

(check-set-mode! 'report-failed)

(define base-dir
  (path->string (path-join (path-temp-dir) "gf-skills-discovery-test"))
) ;define

(define user-dir (path->string (path-join (path base-dir) "user-skills")))

(define pdf-dir (path-join (path user-dir) "pdf-tools"))

(define admin-dir (path-join (path user-dir) "admin-task"))

(when (file-exists? base-dir)
  (rmtree base-dir)
) ;when

(path-mkdir pdf-dir :parents #t :exist-ok #t)
(path-mkdir admin-dir :parents #t :exist-ok #t)
(path-mkdir (path-join pdf-dir "references") :parents #t :exist-ok #t)
(path-mkdir (path-join pdf-dir "scripts") :parents #t :exist-ok #t)
(path-mkdir (path-join (path user-dir) ".hidden-skill")
  :parents  #t
  :exist-ok #t
) ;path-mkdir
(path-mkdir (path-join (path user-dir) "node_modules" "bad-skill")
  :parents  #t
  :exist-ok #t
) ;path-mkdir

(path-write-text (path-join pdf-dir "SKILL.md")
  "---\ndescription: 用户 PDF 工具\n---\n# User PDF"
) ;path-write-text

;; admin-task 未写 name：回退父目录名 admin-task
(path-write-text (path-join admin-dir "SKILL.md")
  "---\ndescription: 运维敏感指令\ndisable-model-invocation: true\n---\n# Admin"
) ;path-write-text

;; 边界保护：references 下的 md 绝不作为技能加载
(path-write-text (path-join pdf-dir "references" "formats.md")
  "---\nname: format-ref\ndescription: ref\n---\nref"
) ;path-write-text

(path-write-text (path-join (path user-dir) ".hidden-skill" "SKILL.md")
  "---\nname: hidden-skill\ndescription: should be skipped\n---\nbody"
) ;path-write-text

;; 项目定制版 pdf-tools（同名冲突测试项）

(define proj-dir (path->string (path-join (path base-dir) "project-skills")))

(define proj-pdf-dir (path-join (path proj-dir) "pdf-tools"))
(path-mkdir proj-pdf-dir :parents #t :exist-ok #t)
(path-write-text (path-join proj-pdf-dir "SKILL.md")
  "---\nname: pdf-tools\ndescription: 项目定制版 PDF 工具\n---\n# Proj PDF"
) ;path-write-text

;; 1. 单目录扫描、边界保护、node_modules/隐藏目录跳过与父目录名回退
(let ((skills (pi-load-skills :skill-paths (list user-dir) :include-defaults #f)))
  (check (length skills) => 2)
  (let ((names (map pi-skill-name skills)))
    (check-true (pair? (member "pdf-tools" names)))
    (check-true (pair? (member "admin-task" names)))
    (check-false (member "bad-skill" names))
    (check-false (member "hidden-skill" names))
    ;; 边界保护：子目录 references/formats.md 绝不作为技能加载
    (check-false (member "format-ref" names))
  ) ;let

  (let ((admin (find (lambda (s) (string=? (pi-skill-name s) "admin-task")) skills)))
    (check-true (pi-skill? admin))
    (check (pi-skill-description admin) => "运维敏感指令")
    (check-true (pi-skill-disable-model-invocation? admin))
  ) ;let
) ;let

;; 2. 同名冲突：Winner / Loser 策略（proj-dir 在前，项目定制版胜出）
(let ((skills (pi-load-skills :skill-paths (list proj-dir user-dir) :include-defaults #f)
      ) ;skills
     ) ;
  ;; 两个目录共 3 个技能定义，pdf-tools 冲突去重后唯 2
  (check (length skills) => 2)
  (let ((pdf (find (lambda (s) (string=? (pi-skill-name s) "pdf-tools")) skills)))
    (check-true (pi-skill? pdf))
    (check (pi-skill-description pdf) => "项目定制版 PDF 工具")
    (check-true (string-contains? (pi-skill-file-path pdf) "project-skills"))
  ) ;let
) ;let

(rmtree base-dir)

(check-report)
