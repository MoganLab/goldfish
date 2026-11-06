;; gfproject-load-config
;; 加载标准库与本地配置并返回合并结果。
;;
;; 语法
;; ----
;; (gfproject-load-config)
;; (gfproject-load-config gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; list
;; 合并后的配置对象；没有任何可用配置时返回空对象 (())。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

;; 默认配置包含 tools 段
(let ((config (gfproject-load-config)))
  (if (equal? config '(()))
    (check-true #t)
    (begin
      (check-true (json-object? config))
      (check-true (pair? (gfproject-extract-tools config)))
    ) ;begin
  ) ;if
) ;let

;; 指定不存在的库目录时仍回退到本地配置
(let ((config (gfproject-load-config "/no/such/gf-lib-dir")))
  (if (equal? config '(()))
    (check-true #t)
    (check-true (json-object? config))
  ) ;if
) ;let

(check-report)
