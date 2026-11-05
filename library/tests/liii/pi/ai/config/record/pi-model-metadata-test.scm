;; pi-model-metadata
;; 构造模型元数据 record 对象。
;;
;; 语法
;; ----
;; (pi-model-metadata id name provider base-url context-window max-tokens matched?)
;;
;; 参数
;; ----
;; id : string?
;; 模型标识。
;;
;; name : string?
;; 模型显示名称。
;;
;; provider : string?
;; 服务商名称。
;;
;; base-url : string?
;; 端点地址。
;;
;; context-window : integer?
;; 上下文窗口大小。
;;
;; max-tokens : integer?
;; 最大输出 token 数。
;;
;; matched? : boolean?
;; 是否匹配上已知模型。
;;
;; 返回值
;; ----
;; pi-model-metadata?
;; 模型元数据 record 实例。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(let ((m (pi-model-metadata "gemini-3.8-flash" "Gemini 3.8 Flash" "google"
           "https://generativelanguage.googleapis.com" 1000000 65536 #t
         ) ;pi-model-metadata
      ) ;m
     ) ;
  (check-true (pi-model-metadata? m))
  (check (pi-model-metadata-id m) => "gemini-3.8-flash")
  (check (pi-model-metadata-name m) => "Gemini 3.8 Flash")
  (check (pi-model-metadata-provider m) => "google")
  (check (pi-model-metadata-context-window m) => 1000000)
  (check (pi-model-metadata-max-tokens m) => 65536)
  (check (pi-model-metadata-matched? m) => #t)
) ;let

(check-report)
