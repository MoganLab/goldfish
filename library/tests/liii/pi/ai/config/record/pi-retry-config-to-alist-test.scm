;; pi-retry-config->alist
;; 将重试配置转换为关联列表。
;;
;; 语法
;; ----
;; (pi-retry-config->alist r)
;;
;; 参数
;; ----
;; r : any?
;; 重试配置 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 enabled、maxRetries 等字段的关联列表。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((cfg (pi-retry-config #t 3 2000 60000)) (al (pi-retry-config->alist cfg)))
  (check-true (pair? al))
  (check (cdr (assoc 'enabled al)) => #t)
  (check (cdr (assoc 'max-retries al)) => 3)
) ;let*

(check-report)
