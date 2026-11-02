;; pi-tool-definition->alist
;; 将工具定义转换为关联列表。
;;
;; 语法
;; ----
;; (pi-tool-definition->alist def)
;;
;; 参数
;; ----
;; def : any?
;; 工具定义 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 name、description、parameters、execute、snippet、guidelines 的关联列表。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let* ((def (pi-tool-definition "tool" "desc" '() (lambda () #t) "snip" '("g1")))
       (al (pi-tool-definition->alist def))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'name al)) => "tool")
  (check (cdr (assoc 'description al)) => "desc")
  (check (cdr (assoc 'snippet al)) => "snip")
) ;let*

(check-report)
