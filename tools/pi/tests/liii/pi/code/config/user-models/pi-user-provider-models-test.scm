;; pi-user-provider-models
;; 返回 provider 的可用模型列表，按 provider 文件中的 models 白名单过滤。
;;
;; 语法
;; ----
;; (pi-user-provider-models config-dir p-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;;
;; 返回值
;; ----
;; list
;; 可用模型的 alist 列表。

(import (liii check) (liii pi code config user-models))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

;; models 白名单只留 k3.1-pro
(let ((models (pi-user-provider-models (string-append FIXTURE "/full") "kimi-coding")))
  (check (length models) => 1)
  (check (cdr (assoc "id" (car models))) => "k3.1-pro")
) ;let

;; 无白名单时使用内置目录全量
(check (> (length (pi-user-provider-models (string-append FIXTURE "/full") "google")) 10)
  =>
  #t
) ;check

;; ollama 无白名单时返回内置模型列表
(let ((models (pi-user-provider-models (string-append FIXTURE "/full") "ollama")))
  (check-true (pair? models))
  (check (cdr (assoc "id" (car models))) => "qwen2.5-coder:latest")
) ;let

(check-report)
