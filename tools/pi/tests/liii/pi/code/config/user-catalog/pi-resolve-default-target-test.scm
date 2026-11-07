;; pi-resolve-default-target
;; 从 settings 解析默认 provider 与模型，支持 provider/model 斜杠语法。
;;
;; 语法
;; ----
;; (pi-resolve-default-target settings)
;;
;; 参数
;; ----
;; settings : pi-settings
;; 全局设置 record。
;;
;; 返回值
;; ----
;; values
;; 两个值：生效的 provider 与模型 id。

(import (liii check)
  (liii pi ai config record)
  (liii pi code config user-catalog)
) ;import

(check-set-mode! 'report-failed)

;; 普通 provider/model
(let-values (((p m) (pi-resolve-default-target (pi-settings "kimi-coding" "k3.1-pro"))))
  (check p => "kimi-coding")
  (check m => "k3.1-pro")
) ;let-values

;; provider/model 斜杠语法
(let-values (((p m) (pi-resolve-default-target (pi-settings "google" "kimi-coding/k3"))))
  (check p => "kimi-coding")
  (check m => "k3")
) ;let-values

;; 缺省值回退
(let-values (((p m) (pi-resolve-default-target (pi-settings))))
  (check p => "google")
  (check m => "gemini-3.8-flash")
) ;let-values

(check-report)
