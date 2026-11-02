;; pi-block->alist
;; 将内容块对象转换为关联列表。
;;
;; 语法
;; ----
;; (pi-block->alist block)
;;
;; 参数
;; ----
;; block : any?
;; 内容块 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 type 及对应内容的关联列表。

(import (liii check) (liii pi ai message record))

(check-set-mode! 'report-failed)

(let* ((b (pi-text-block "txt")) (al (pi-block->alist b)))
  (check-true (pair? al))
  (check (cdr (assoc 'type al)) => "text")
  (check (cdr (assoc 'text al)) => "txt")
) ;let*

(check-report)
