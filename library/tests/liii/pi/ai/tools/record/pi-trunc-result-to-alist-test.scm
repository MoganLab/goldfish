;; pi-trunc-result->alist
;; 将文本截断结果转换为关联列表。
;;
;; 语法
;; ----
;; (pi-trunc-result->alist r)
;;
;; 参数
;; ----
;; r : any?
;; 截断结果 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 content、truncated、total-lines、total-bytes 等字段的关联列表。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let* ((tr (pi-trunc-result "body" #t 'lines 10 100 5 50 #f #f))
       (al (pi-trunc-result->alist tr))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'content al)) => "body")
  (check (cdr (assoc 'truncated al)) => #t)
  (check (cdr (assoc 'total-lines al)) => 10)
) ;let*

(check-report)
