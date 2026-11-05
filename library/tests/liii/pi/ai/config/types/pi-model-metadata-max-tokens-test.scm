;; pi-model-metadata-max-tokens
;; 获取模型规格元数据 record 的 max-tokens 字段。
;;
;; 语法
;; ----
;; (pi-model-metadata-max-tokens obj)
;;
;; 参数
;; ----
;; obj : any?
;; 模型规格元数据 record 实例。
;;
;; 返回值
;; ----
;; integer?
;; max-tokens 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-model-metadata "gemini-2.5-flash" "Gemini 2.5 Flash"
                "google" "https://generativelanguage.googleapis.com" 1048576
                65536 #t
              ) ;%make-pi-model-metadata
       ) ;obj-a
       (obj-b (%make-pi-model-metadata "deepseek-v4" "DeepSeek V4" "deepseek" #f
                128000 8192 #f
              ) ;%make-pi-model-metadata
       ) ;obj-b
      ) ;
  (check (pi-model-metadata-max-tokens obj-a) => 65536)
  (check (pi-model-metadata-max-tokens obj-b) => 8192)
) ;let*

(check-report)
