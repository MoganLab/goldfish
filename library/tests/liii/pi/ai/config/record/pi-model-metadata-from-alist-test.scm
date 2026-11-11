;; pi-model-metadata-from-alist
;; 将 alist 形式的模型元数据规范化为 record。
;;
;; 语法
;; ----
;; (pi-model-metadata-from-alist m)
;;
;; 参数
;; ----
;; m : any?
;; pi-model-metadata record 或 alist 形式的模型元数据。
;;
;; 返回值
;; ----
;; (or pi-model-metadata? boolean?)
;; record 输入原样返回；alist 输入转换为 record（缺失字段用安全默认值补齐）；
;; 其余输入返回 #f。

(import (liii check) (liii pi ai config record))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-model-metadata-from-alist))

;; 1. record 输入原样返回
(let* ((m (pi-model-metadata "gemini-3.8-flash" "Gemini 3.8 Flash" "google"
            "https://generativelanguage.googleapis.com" 1000000 65536 #t
          ) ;pi-model-metadata
       ) ;m
       (out (pi-model-metadata-from-alist m))
      ) ;
  (check-true (pi-model-metadata? out))
  (check (eq? out m) => #t)
  (check (pi-model-metadata-id out) => "gemini-3.8-flash")
) ;let*

;; 2. 完整 alist 转 record
(let ((out (pi-model-metadata-from-alist
             '((id . "deepseek-chat")
               (name . "DeepSeek Chat")
               (provider . "deepseek")
               (base-url . "https://api.deepseek.com")
               (context-window . 128000)
               (max-tokens . 8192)
               (matched? . #t)
             ) ;alist
           ) ;pi-model-metadata-from-alist
      ) ;out
     ) ;
  (check-true (pi-model-metadata? out))
  (check (pi-model-metadata-id out) => "deepseek-chat")
  (check (pi-model-metadata-provider out) => "deepseek")
  (check (pi-model-metadata-base-url out) => "https://api.deepseek.com")
  (check (pi-model-metadata-context-window out) => 128000)
  (check (pi-model-metadata-max-tokens out) => 8192)
) ;let

;; 3. 缺失字段的 alist：安全默认值补齐
(let ((out (pi-model-metadata-from-alist
             '((base-url . "http://gateway:8080")
               (max-tokens . 1024)
             ) ;alist
           ) ;pi-model-metadata-from-alist
      ) ;out
     ) ;
  (check-true (pi-model-metadata? out))
  (check (pi-model-metadata-base-url out) => "http://gateway:8080")
  (check (pi-model-metadata-max-tokens out) => 1024)
  (check (pi-model-metadata-id out) => "")
  (check (pi-model-metadata-context-window out) => #f)
  (check (pi-model-metadata-matched? out) => #t)
) ;let

;; 4. 非法输入返回 #f
(check (pi-model-metadata-from-alist "garbage") => #f)
(check (pi-model-metadata-from-alist 42) => #f)
(check (pi-model-metadata-from-alist '()) => #f)

(check-report)
