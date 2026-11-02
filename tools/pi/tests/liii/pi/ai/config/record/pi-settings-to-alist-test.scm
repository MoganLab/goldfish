;; pi-settings->alist
;; 将客户端全局偏好配置转换为关联列表。
;;
;; 语法
;; ----
;; (pi-settings->alist s)
;;
;; 参数
;; ----
;; s : any?
;; 全局偏好配置 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 defaultProvider、defaultModel 等字段的关联列表。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((s (pi-settings "deepseek" "deepseek-v4-pro" "off" #f #f '()))
       (al (pi-settings->alist s))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'default-provider al)) => "deepseek")
  (check (cdr (assoc 'default-model al)) => "deepseek-v4-pro")
) ;let*

(check-report)
