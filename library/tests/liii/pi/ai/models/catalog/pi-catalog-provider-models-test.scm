;; pi-catalog-provider-models
;; 获取指定 provider 的模型列表。
;;
;; 语法
;; ----
;; (pi-catalog-provider-models catalog provider)
;;
;; 参数
;; ----
;; catalog : list?
;; 模型目录关联表。
;;
;; provider : string? 或 symbol?
;; 服务商标识符。
;;
;; 返回值
;; ----
;; list?
;; 该服务商包含的模型配置列表。

(import (liii check) (liii pi ai models catalog))

(check-set-mode! 'report-failed)

(let* ((catalog
         (list
           (cons "provider-a"
             (list
               (cons "models" (list (list (cons "id" "a-1")) (list (cons "id" "a-2"))))
             ) ;list
           ) ;cons
           (cons "provider-b" (list (cons "models" (vector (list (cons "id" "b-1"))))))
         ) ;list
       ) ;catalog
      ) ;
  ;; 字符串与符号查询
  (check (length (pi-catalog-provider-models catalog "provider-a")) => 2)
  (check (length (pi-catalog-provider-models catalog 'provider-a)) => 2)
  ;; vector 形式模型列表自动转 list
  (check (length (pi-catalog-provider-models catalog "provider-b")) => 1)
  ;; 不存在的 provider 返回空列表
  (check (pi-catalog-provider-models catalog "provider-c") => '())
) ;let*

(check-report)
