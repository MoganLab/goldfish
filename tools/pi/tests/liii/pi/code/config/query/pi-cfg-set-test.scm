;; pi-cfg-set
;; 返回更新指定键后的新配置对象（不修改原配置）。
;;
;; 语法
;; ----
;; (pi-cfg-set cfg key val) -> alist
;;
;; 参数
;; ----
;; cfg : alist
;; 原运行时配置。
;;
;; key : symbol
;; 配置键名。
;;
;; val : any
;; 写入的取值。
;;
;; 返回值
;; ------
;; alist
;; 更新后的新配置对象。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(let* ((cfg '((provider . "google")))
       (cfg2 (pi-cfg-set cfg 'model-id "gemini-2.5-flash"))
       (cfg3 (pi-cfg-set cfg2 'provider "deepseek"))
      ) ;
  (check (pi-cfg-ref cfg 'provider) => "google")
  (check (pi-cfg-ref cfg 'model-id) => #f)
  (check (pi-cfg-ref cfg2 'provider) => "google")
  (check (pi-cfg-ref cfg2 'model-id) => "gemini-2.5-flash")
  (check (pi-cfg-ref cfg3 'provider) => "deepseek")
  (check (pi-cfg-ref cfg3 'model-id) => "gemini-2.5-flash")
) ;let*

(check-report)
