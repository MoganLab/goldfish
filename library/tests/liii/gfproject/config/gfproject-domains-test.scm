;; gfproject-domains
;; 获取项目配置中的 domains 字典对象。
;;
;; 语法
;; ----
;; (gfproject-domains)
;; (gfproject-domains gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; list
;; domains 字典对象；未配置时返回空列表 ()。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

;; 本项目配置了 domains
(let ((domains (gfproject-domains)))
  (check-true (json-object? domains))
  (check-true (json-contains-key? domains "r7rs"))
  (check-true (json-contains-key? domains "goldfish"))
  (check-true (json-contains-key? domains "library"))
) ;let

;; 指定不存在的库目录时返回空列表
(let ((domains (gfproject-domains "/no/such/gf-lib-dir")))
  (check (list? domains) => #t)
) ;let

(check-report)
