;; gfproject-load-config-bundle
;; 加载标准库与本地配置并合并为配置包。
;;
;; 语法
;; ----
;; (gfproject-load-config-bundle)
;; (gfproject-load-config-bundle gf-lib)
;;
;; 参数
;; ----
;; gf-lib : string (可选)
;; 标准库目录路径；缺省时使用默认标准库目录。
;;
;; 返回值
;; ----
;; list
;; 配置包 alist，含 lib_config、local_config、lib_path、local_path、
;; merged_config、lib_tools、local_tools、merged_tools 八个键。

(import (liii check) (liii gfproject config) (liii json))

(check-set-mode! 'report-failed)

(let ((bundle (gfproject-load-config-bundle)))
  ;; 八个键齐备
  (check-true (pair? (bundle-ref bundle "lib_config")))
  (check-true (pair? (bundle-ref bundle "lib_tools")))
  (check-true (pair? (bundle-ref bundle "local_tools")))
  (check-true (pair? (bundle-ref bundle "merged_tools")))
  (check-true (pair? (bundle-ref bundle "merged_config")))
  ;; 路径键为字符串或 #f
  (let ((lib-path (bundle-ref bundle "lib_path")))
    (if lib-path (check-true (string? lib-path)) (check-true #t))
  ) ;let
  (let ((local-path (bundle-ref bundle "local_path")))
    (if local-path (check-true (string? local-path)) (check-true #t))
  ) ;let
) ;let

;; 指定不存在的库目录时仍可返回配置包
(let ((bundle (gfproject-load-config-bundle "/no/such/gf-lib-dir")))
  (check-true (pair? bundle))
  (check (bundle-ref bundle "lib_config") => '(()))
) ;let

(check-report)
