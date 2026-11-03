;; pi-model-metadata-matched?
;; 获取模型规格元数据 record 的 matched? 字段。
;;
;; 语法
;; ----
;; (pi-model-metadata-matched? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 模型规格元数据 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; matched? 字段的值。

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
  (check (pi-model-metadata-matched? obj-a) => #t)
  (check (pi-model-metadata-matched? obj-b) => #f)
) ;let*

(check-report)
