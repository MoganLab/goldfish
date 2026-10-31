(import (liii check)
  (liii pi code cli args)
  (liii pi code config)
  (liii pi ai config record)
) ;import

(check-set-mode! 'report-failed)

;; resolve-pi-config 读取用户配置目录（providers.json + models/）：
;; 白名单外的 provider/模型不可路由；provider 文件的 baseUrl/api 对实际请求生效；
;; 模型文件字段覆盖内置目录。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"))

(define FULL-DIR (string-append FIXTURE "/full"))
(define EMPTY-DIR (string-append FIXTURE "/no-providers"))

;; 1. 白名单内的模型可路由；provider 文件的 baseUrl 生效
(let* ((args (parse-pi-args '("--model" "k3.1-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("KIMI_API_KEY" . "kimi-key-test")))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "k3.1-pro")
  (check (cdr (assoc 'api-key cfg)) => "kimi-key-test")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)
    ;; provider 文件写的 baseUrl（models/kimi-coding.json）
    (check (pi-model-metadata-base-url meta) => "https://api.kimi.com/coding/v1")
    ;; 模型文件（models/kimi-coding/k3.1-pro.json）字段
    (check (pi-model-metadata-context-window meta) => 1048576)
    (check (pi-model-metadata-max-tokens meta) => 131072)))

;; 2. 白名单外的模型不可路由（kimi-coding 的 models 白名单只含 k3.1-pro，
;;    内置目录中的 k3 不可用）
(check-catch 'pi-config-error
  (resolve-pi-config (parse-pi-args '("--provider" "kimi-coding" "--model" "k3"))
    `(("PI_CODING_AGENT_DIR" . ,FULL-DIR))
  ) ;resolve-pi-config
) ;check-catch

;; 2.1 无 models 白名单的 provider 也可用其内置目录模型（google）
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-3.8-flash")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google"))

;; 3. 白名单外的 provider 不可路由（deepseek 有内置定义，但不在 providers.json）
(check-catch 'pi-config-error
  (resolve-pi-config (parse-pi-args '("--provider" "deepseek"))
    `(("PI_CODING_AGENT_DIR" . ,FULL-DIR))
  ) ;resolve-pi-config
) ;check-catch

;; 4. 纯自定义 provider（无内置定义）可路由到其目录中的模型
(let* ((args (parse-pi-args '("--model" "m1")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "custom-kimi")
  (check (cdr (assoc 'model-id cfg)) => "m1")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)
    (check (pi-model-metadata-base-url meta) => "https://mirror.example.com/coding")
    (check (pi-model-metadata-context-window meta) => 262144)
    (check (pi-model-metadata-max-tokens meta) => 32768)))

;; 5. 无 providers.json：全部内置 provider 可用
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,EMPTY-DIR)))
       (cfg (resolve-pi-config args env)))
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-pro")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)))

;; 6. providers.json 语法错误时拒绝启动
(check-catch 'parse-error
  (resolve-pi-config (parse-pi-args '())
    `(("PI_CODING_AGENT_DIR" . ,(string-append FIXTURE "/bad-providers")))
  ) ;resolve-pi-config
) ;check-catch

(check-report)
