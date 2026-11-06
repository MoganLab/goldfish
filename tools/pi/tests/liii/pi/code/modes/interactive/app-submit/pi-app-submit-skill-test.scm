;; pi-app-submit!
;; 提交 /skill:<name> 命令：进入执行队列前经 expand-skill-command 展开
;; 为完整技能块并附加用户参数。
;;
;; 语法
;; ----
;; (pi-app-submit! app text :mode mode) -> 未指定
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; text : string
;; 用户输入文本（/skill:name [args]）。
;; mode : symbol
;; 提交模式（auto/steering/follow-up）。
;;
;; 返回值
;; ------
;; 未指定

(import
  (scheme base)
  (liii check)
  (liii tui)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii pi code core skills types)
  (liii pi code modes interactive app))

(check-set-mode! 'report-failed)

(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (tmp (path-temp-dir))
       (skill-file (path-join tmp "pdf-SKILL.md"))
       (submitted #f))
  (path-write-text skill-file "---
name: pdf-tools
description: 提取 PDF 文本
---
# PDF Instructions
Extract text.")
  (let* ((skill (%make-pi-skill "pdf-tools" "提取 PDF 文本" (path->string skill-file) (path->string tmp) #f))
         (app (make-pi-app :terminal vterm
                           :skills (list skill)
                           :handler (lambda (a m) (set! submitted m)))))
    ;; 提交 /skill:pdf-tools doc.pdf
    (pi-app-submit! app "/skill:pdf-tools doc.pdf")
    ;; 验证消息已展开
    (check-true (string? submitted))
    (check-true (string-starts? submitted "<skill name=\"pdf-tools\" location=\""))
    (check-true (string-contains? submitted "References are relative to "))
    (check-true (string-contains? submitted "# PDF Instructions\nExtract text."))
    (check-true (string-ends? submitted "</skill>\n\ndoc.pdf"))))

(check-report)
