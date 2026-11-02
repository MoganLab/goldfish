;; pi-auto-activate-default-model!
;; 默认模型未激活时，将其追加进 provider 白名单或模型白名单并落盘。
;;
;; 语法
;; ----
;; (pi-auto-activate-default-model! config-dir settings)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; settings : pi-settings
;; 全局设置 record，读取 defaultProvider 与 defaultModel。
;;
;; 返回值
;; ----
;; boolean
;; 执行了写入返回 #t；无需激活或模型不存在返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai config record)
  (liii pi code config user-catalog)
) ;import

(check-set-mode! 'report-failed)

(define DIR (path->string (path-join (path-temp-dir) "pi-auto-activate-default-model-test")))
(define MODELS (path->string (path-join (path DIR) "models")))
(define PROVIDERS (path->string (path-join (path MODELS) "providers.json")))
(define KIMI (path->string (path-join (path MODELS) "kimi-coding.json")))

(when (file-exists? DIR)
  (rmtree DIR #t))
(path-mkdir MODELS :parents #t :exist-ok #t)
(path-write-text PROVIDERS "[\"kimi-coding\"]\n")
(path-write-text KIMI
  "{\"name\":\"Kimi Coding\",\"models\":[\"k3.1-pro\"]}\n"
) ;path-write-text

;; 默认模型 k3 属于内置目录但不在 provider 白名单中：追加并落盘
(check (pi-auto-activate-default-model! DIR (pi-settings "kimi-coding" "k3")) => #t)
(check (string-contains (path-read-text KIMI) "\"k3\"") => #t)
(check (string-contains (path-read-text KIMI) "\"k3.1-pro\"") => #t)

;; 已经激活后再次调用不再写入
(check (pi-auto-activate-default-model! DIR (pi-settings "kimi-coding" "k3")) => #f)

;; 不存在的模型不写入
(check (pi-auto-activate-default-model! DIR (pi-settings "kimi-coding" "no-such-model")) => #f)

(rmtree DIR #t)

(check-report)
