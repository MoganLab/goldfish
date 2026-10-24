(import (liii check)
        (liii pi ai models catalog))

(check-set-mode! 'report-failed)

;; pi-catalog-merge-provider-models
;; 按模型 id 进行覆盖合并：overlay 覆盖 base 中的同 id 模型，保留 base 中未冲突的模型

(let* ((base
        (list
          (list (cons "id" "m1") (cons "name" "Model One Base"))
          (list (cons "id" "m2") (cons "name" "Model Two Base"))))
       (overlay
        (list
          (list (cons "id" "m2") (cons "name" "Model Two Overlay"))
          (list (cons "id" "m3") (cons "name" "Model Three New"))))
       (merged (pi-catalog-merge-provider-models base overlay)))
  (check (length merged) => 3)
  ;; m2 应该被 overlay 覆盖
  (let ((m2 (assoc "m2" (map (lambda (m) (cons (cdr (assoc "id" m)) m)) merged))))
    (check-true (pair? m2))
    (check (cdr (assoc "name" (cdr m2))) => "Model Two Overlay"))
  ;; m1 应该保留 base
  (let ((m1 (assoc "m1" (map (lambda (m) (cons (cdr (assoc "id" m)) m)) merged))))
    (check-true (pair? m1))
    (check (cdr (assoc "name" (cdr m1))) => "Model One Base"))
  ;; m3 应该加入
  (let ((m3 (assoc "m3" (map (lambda (m) (cons (cdr (assoc "id" m)) m)) merged))))
    (check-true (pair? m3))
    (check (cdr (assoc "name" (cdr m3))) => "Model Three New")))

;; 边界情况：空 base 或空 overlay
(let ((models (list (list (cons "id" "m1")))))
  (check (pi-catalog-merge-provider-models '() models) => models)
  (check (pi-catalog-merge-provider-models models '()) => models))

(check-report)
