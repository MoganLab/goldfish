;; %make-pi-model-metadata
;; 构造模型规格元数据 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-model-metadata id name provider base-url context-window max-tokens matched?)
;;
;; 参数
;; ----
;; id : string?
;; 模型标识符
;;
;; name : string?
;; 模型显示名称
;;
;; provider : string?
;; 所属服务商名称
;;
;; base-url : string? 或 #f
;; API 基础地址
;;
;; context-window : integer?
;; 上下文窗口 token 数
;;
;; max-tokens : integer?
;; 单次输出最大 token 数
;;
;; matched? : boolean?
;; 是否与内置模型表匹配成功
;;
;; 返回值
;; ----
;; pi-model-metadata?
;; 模型规格元数据 record 实例。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-model-metadata "gemini-2.5-flash" "Gemini 2.5 Flash"
             "google" "https://generativelanguage.googleapis.com" 1048576 65536
             #t
           ) ;%make-pi-model-metadata
      ) ;obj
     ) ;
  (check-true (pi-model-metadata? obj))
  (check (pi-model-metadata-id obj) => "gemini-2.5-flash")
  (check (pi-model-metadata-name obj) => "Gemini 2.5 Flash")
  (check (pi-model-metadata-provider obj) => "google")
  (check (pi-model-metadata-base-url obj)
    =>
    "https://generativelanguage.googleapis.com"
  ) ;check
  (check (pi-model-metadata-context-window obj) => 1048576)
  (check (pi-model-metadata-max-tokens obj) => 65536)
  (check (pi-model-metadata-matched? obj) => #t)
) ;let

(check-report)
