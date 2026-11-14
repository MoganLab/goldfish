;; pi-cfg-merge
;; 批量覆盖多个配置键并返回新配置对象。
;;
;; 语法
;; ----
;; (pi-cfg-merge cfg overrides) -> alist
;;
;; 参数
;; ----
;; cfg : alist
;; 原运行时配置。
;;
;; overrides : alist
;; 待覆盖的键值对列表。
;;
;; 返回值
;; ------
;; alist
;; 合并后的新配置对象。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(let* ((cfg '((provider . "google") (model-id . "m1") (timeout . 60)))
       (merged (pi-cfg-merge cfg '((provider . "deepseek") (api-key . "sk-1"))))
      ) ;
  (check (pi-cfg-ref merged 'provider) => "deepseek")
  (check (pi-cfg-ref merged 'model-id) => "m1")
  (check (pi-cfg-ref merged 'timeout) => 60)
  (check (pi-cfg-ref merged 'api-key) => "sk-1")
) ;let*

(check-report)
