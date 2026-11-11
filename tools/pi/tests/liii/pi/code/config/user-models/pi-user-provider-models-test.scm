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

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code config user-models)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

;; models 白名单只留 k3.1-pro
(let ((models (pi-user-provider-models (string-append FIXTURE "/full") "kimi-coding"))
     ) ;
  (check (length models) => 1)
  (check (cdr (assoc "id" (car models))) => "k3.1-pro")
) ;let

;; 无白名单时使用内置目录全量
(check
  (> (length (pi-user-provider-models (string-append FIXTURE "/full") "google"))
    10
  ) ;>
  =>
  #t
) ;check

;; ollama 无白名单时返回内置模型列表
(let ((models (pi-user-provider-models (string-append FIXTURE "/full") "ollama")))
  (check-true (pair? models))
  (check (cdr (assoc "id" (car models))) => "qwen2.5-coder:latest")
) ;let

;; all-in-one 动态链式模型映射
(let ((tmp-dir (path->string (path-join (path-temp-dir) "pi-aio-prov-models-test"))))
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir #t)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join tmp-dir "all-in-one.json"))
    "{\"provider\":\"all-in-one\",\"chains\":[{\"id\":\"coding-deep\",\"name\":\"深度链\",\"nodes\":[{\"provider\":\"p\",\"model\":\"m\"}]}]}"
  ) ;path-write-text
  (let ((models (pi-user-provider-models tmp-dir "all-in-one")))
    (check (length models) => 1)
    (check (cdr (assoc "id" (car models))) => "coding-deep")
    (check (cdr (assoc "provider" (car models))) => "all-in-one")
    (check (cdr (assoc "api" (car models))) => "all-in-one")
  ) ;let
  (rmtree tmp-dir #t)
) ;let

(check-report)
