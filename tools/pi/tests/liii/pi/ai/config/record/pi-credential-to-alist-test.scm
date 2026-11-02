;; pi-credential->alist
;; 将凭据对象转换为关联列表。
;;
;; 语法
;; ----
;; (pi-credential->alist c)
;;
;; 参数
;; ----
;; c : any?
;; 凭据 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 provider、key 与 type 字段的关联列表。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((c (pi-credential "google" "secret-key" "api_key"))
       (al (pi-credential->alist c))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'provider al)) => "google")
  (check (cdr (assoc 'key al)) => "secret-key")
  (check (cdr (assoc 'type al)) => "api_key")
) ;let*

(check-report)
