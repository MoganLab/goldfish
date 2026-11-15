;; pi-cfg->alist
;; 将 <pi-config> 记录转换为关联表（alist），供下层模块使用。
;;
;; 语法
;; ----
;; (pi-cfg->alist cfg) -> alist
;;
;; 参数
;; ----
;; cfg : <pi-config> | alist
;; 运行时配置记录或关联表。
;;
;; 返回值
;; ------
;; alist
;; 包含标准字段与扩展字段的关联表。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(let* ((cfg (pi-cfg-set (make-pi-config :provider "deepseek" :model-id "m1") 'emit 'fn))
       (al (pi-cfg->alist cfg))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'provider al)) => "deepseek")
  (check (cdr (assoc 'model-id al)) => "m1")
  (check (cdr (assoc 'emit al)) => 'fn)
  (check (pi-cfg->alist '((provider . "google"))) => '((provider . "google")))
) ;let*

(check-report)
