;; pi-tool-result->alist
;; 将工具执行结果转换为关联列表。
;;
;; 语法
;; ----
;; (pi-tool-result->alist res)
;;
;; 参数
;; ----
;; res : any?
;; 工具执行结果 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 content 与 isError 的关联列表。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let* ((res (pi-tool-result "data" #f)) (al (pi-tool-result->alist res)))
  (check-true (pair? al))
  (check (cdr (assoc 'isError al)) => #f)
) ;let*

(check-report)
