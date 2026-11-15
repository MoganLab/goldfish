;; pi-cfg-merge
;; 批量覆盖多个配置键并返回新配置对象。
;;
;; 语法
;; ----
;; (pi-cfg-merge cfg overrides) -> <pi-config> | alist
;;
;; 参数
;; ----
;; cfg : <pi-config> | alist
;; 原运行时配置。
;;
;; overrides : alist
;; 待覆盖的键值对列表。
;;
;; 返回值
;; ------
;; <pi-config> | alist
;; 合并后的新配置对象。

(import (liii check) (liii pi ai config runtime))

(check-set-mode! 'report-failed)

(let* ((cfg (make-pi-config :provider "google" :model-id "m1"))
       (merged (pi-cfg-merge cfg '((provider . "deepseek") (api-key . "sk-1"))))
      ) ;
  (check (pi-cfg-ref merged 'provider) => "deepseek")
  (check (pi-cfg-ref merged 'model-id) => "m1")
  (check (pi-cfg-ref merged 'api-key) => "sk-1")
) ;let*

(check-report)
