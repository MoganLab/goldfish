;; pi-validate-skill-name
;; 校验技能名称是否符合 Agent Skills 规范：仅小写 a-z、数字 0-9、短横线，
;; 长度 1 到 64 字符，禁止首尾短横线与连续双短横线。
;;
;; 语法
;; ----
;; (pi-validate-skill-name name) -> list
;;
;; 参数
;; ----
;; name : any
;; 待校验的名称。
;;
;; 返回值
;; ------
;; list
;; 错误消息列表；合法时为空列表。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

;; 合法名称
(check (pi-validate-skill-name "valid-name") => '())
(check (pi-validate-skill-name "skill-123") => '())
(check (pi-validate-skill-name "a") => '())

;; 非字符串
(check (pi-validate-skill-name 42) => (list "name must be a string"))

;; 空名称
(let ((errs (pi-validate-skill-name "")))
  (check-true (pair? (member "name is required" errs)))
) ;let

;; 非法字符：大写、下划线、中文
(let ((errs (pi-validate-skill-name "Bad_Name")))
  (check-true (pair? (member "name contains invalid characters (must be lowercase a-z, 0-9, hyphens only)"
                       errs
                     ) ;member
              ) ;pair?
  ) ;check-true
) ;let

;; 超长（65 字符）
(let* ((long-name (make-string 65 #\a)) (errs (pi-validate-skill-name long-name)))
  (check-true (pair? (member "name exceeds 64 characters (65)" errs)))
) ;let*

;; 首尾短横线
(let ((errs (pi-validate-skill-name "-leading")))
  (check-true (pair? (member "name must not start or end with a hyphen" errs)))
) ;let
(let ((errs (pi-validate-skill-name "trailing-")))
  (check-true (pair? (member "name must not start or end with a hyphen" errs)))
) ;let

;; 连续双短横线
(let ((errs (pi-validate-skill-name "double--hyphen")))
  (check-true (pair? (member "name must not contain consecutive hyphens" errs)))
) ;let

(check-report)
