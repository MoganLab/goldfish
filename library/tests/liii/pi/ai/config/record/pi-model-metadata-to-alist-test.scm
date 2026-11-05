;; pi-model-metadata->alist
;; 将模型元数据转换为关联列表。
;;
;; 语法
;; ----
;; (pi-model-metadata->alist m)
;;
;; 参数
;; ----
;; m : any?
;; 模型元数据 record 或原生 alist。
;;
;; 返回值
;; ----
;; alist?
;; 包含 id、name、provider、baseUrl 等字段的关联列表。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let* ((m (pi-model-metadata "gemini-3.8-flash" "Gemini 3.8 Flash" "google"
            "https://generativelanguage.googleapis.com" 1000000 65536 #t
          ) ;pi-model-metadata
       ) ;m
       (al (pi-model-metadata->alist m))
      ) ;
  (check-true (pair? al))
  (check (cdr (assoc 'id al)) => "gemini-3.8-flash")
  (check (cdr (assoc 'provider al)) => "google")
  (check (cdr (assoc 'context-window al)) => 1000000)
) ;let*

(check-report)
