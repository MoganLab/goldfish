;; pi-validate-skill-description
;; 校验技能描述：必填非空字符串且不超过 1024 字符。
;;
;; 语法
;; ----
;; (pi-validate-skill-description description) -> list
;;
;; 参数
;; ----
;; description : any
;; 待校验的描述。
;;
;; 返回值
;; ------
;; list
;; 错误消息列表；合法时为空列表。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

;; 合法描述
(check (pi-validate-skill-description "提取 PDF 文本") => '())

;; 非字符串 / 空白
(check (pi-validate-skill-description #f) => (list "description is required"))
(check (pi-validate-skill-description "   ")
  =>
  (list "description is required")
) ;check

;; 超长（1025 字符）
(let ((errs (pi-validate-skill-description (make-string 1025 #\a))))
  (check (length errs) => 1)
  (check-true (pair? (member "description exceeds 1024 characters (1025)" errs)))
) ;let

(check-report)
