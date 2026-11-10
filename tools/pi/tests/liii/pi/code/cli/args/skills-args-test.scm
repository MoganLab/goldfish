;; parse-pi-args
;; 技能参数解析：--skill 与别名 --skills 可重复传入并累加到 skill-paths，
;; --no-skills 彻底禁用技能加载。
;;
;; 语法
;; ----
;; (parse-pi-args args) -> alist
;;
;; 参数
;; ----
;; args : list
;; 命令行参数字符串列表。
;;
;; 返回值
;; ------
;; alist
;; 解析结果（skill-paths 累加、no-skills 布尔）。

(import (scheme base) (liii check) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; 1. 默认状态：skill-paths 为空列表，no-skills 为 #f
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'skill-paths) => '())
  (check (pi-args-ref res 'no-skills) => #f)
) ;let

;; 2. 解析单个 --skill 参数
(let ((res (parse-pi-args '("--skill" "/custom/skills/dir"))))
  (check (pi-args-ref res 'skill-paths) => '("/custom/skills/dir"))
) ;let

;; 3. 解析 --skills 与多个累加路径
(let ((res (parse-pi-args '("--skill" "/path1" "--skills" "/path2"))))
  (check (pi-args-ref res 'skill-paths) => '("/path1" "/path2"))
) ;let

;; 4. 解析 --no-skills 开关
(let ((res (parse-pi-args '("--no-skills"))))
  (check (pi-args-ref res 'no-skills) => #t)
) ;let

(check-report)
