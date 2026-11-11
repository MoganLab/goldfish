;; all-in-one-chains->models
;; 将 <chain-def> 列表映射为模型元数据 alist 列表。
;;
;; 语法
;; ----
;; (all-in-one-chains->models chains)
;;
;; 参数
;; ----
;; chains : list of <chain-def>
;; 链定义记录列表。
;;
;; 返回值
;; ----
;; list of alist
;; 伪模型元数据列表。

(import (scheme base)
  (liii check)
  (liii pi all_in_one types)
  (liii pi all_in_one provider)
) ;import

(check-set-mode! 'report-failed)

(let* ((c1 (make-chain-def "coding-deep" "深度链" '()))
       (c2 (make-chain-def "coding-fast" "极速链" '()))
       (models (all-in-one-chains->models (list c1 c2)))
      ) ;
  (check (length models) => 2)
  (check (cdr (assoc "id" (car models))) => "coding-deep")
  (check (cdr (assoc "id" (cadr models))) => "coding-fast")
  (check (cdr (assoc "provider" (car models))) => "all-in-one")
  (check (cdr (assoc "provider" (cadr models))) => "all-in-one")
) ;let*

(check-report)
