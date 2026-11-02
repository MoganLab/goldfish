;; pi-message->alist
;; 将消息对象转换为标准关联列表。
;;
;; 语法
;; ----
;; (pi-message->alist msg)
;;
;; 参数
;; ----
;; msg : any?
;; 消息 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 role、content 等字段的关联列表。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((msg (pi-user-message "hi")) (al (pi-message->alist msg)))
  (check-true (pair? al))
  (check (cdr (assoc 'role al)) => "user")
) ;let*

(check-report)
