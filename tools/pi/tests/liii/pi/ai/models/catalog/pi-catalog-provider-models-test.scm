(import (liii check)
        (liii pi ai models catalog))

(check-set-mode! 'report-failed)

;; pi-catalog-provider-models
;; 获取指定 provider 的模型列表

(let* ((catalog
        (list
          (cons "provider-a"
                (list (cons "models"
                            (list (list (cons "id" "a-1"))
                                  (list (cons "id" "a-2"))))))
          (cons "provider-b"
                (list (cons "models"
                            (vector (list (cons "id" "b-1")))))))))
  ;; 字符串与符号查询
  (check (length (pi-catalog-provider-models catalog "provider-a")) => 2)
  (check (length (pi-catalog-provider-models catalog 'provider-a)) => 2)
  ;; vector 形式模型列表自动转 list
  (check (length (pi-catalog-provider-models catalog "provider-b")) => 1)
  ;; 不存在的 provider 返回空列表
  (check (pi-catalog-provider-models catalog "provider-c") => '()))

(check-report)
