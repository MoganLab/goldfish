;; resolve-pi-config
;;
;; 根据 CLI 参数、环境变量和配置文件解析完整的运行配置
;;
;; 语法
;; ----
;; (resolve-pi-config parsed-args . opt-env)
;;
;; 参数
;; ----
;; parsed-args : 命令行解析结果 alist
;; opt-env     : 可选的环境变量 alist
;;
;; 返回值
;; ----
;; 解析后的配置 alist

(import (scheme base)
  (scheme time)
  (liii check)
  (liii string)
  (liii path)
  (liii path stat)
  (liii os)
  (liii json)
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
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define FULL-DIR (string-append FIXTURE "/full"))

(define EMPTY-DIR (string-append FIXTURE "/no-providers"))

;; 1. 白名单内的模型可路由；provider 文件的 baseUrl 生效
(let* ((args (parse-pi-args '("--model" "k3.1-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("KIMI_API_KEY" . "kimi-key-test")))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "k3.1-pro")
  (check (cdr (assoc 'api-key cfg)) => "kimi-key-test")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)
    ;; provider 文件写的 baseUrl（models/kimi-coding.json）
    (check (pi-model-metadata-base-url meta) => "https://api.kimi.com/coding/v1")
    ;; 模型文件（models/kimi-coding/k3.1-pro.json）字段
    (check (pi-model-metadata-context-window meta) => 1048576)
    (check (pi-model-metadata-max-tokens meta) => 131072)
  ) ;let
) ;let*

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
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
) ;let*

;; 3. 白名单外的 provider 不可路由（deepseek 有内置定义，但不在 providers.json）
(check-catch 'pi-config-error
  (resolve-pi-config (parse-pi-args '("--provider" "deepseek"))
    `(("PI_CODING_AGENT_DIR" . ,FULL-DIR))
  ) ;resolve-pi-config
) ;check-catch

;; 4. 纯自定义 provider（无内置定义）可路由到其目录中的模型
(let* ((args (parse-pi-args '("--model" "m1")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "custom-kimi")
  (check (cdr (assoc 'model-id cfg)) => "m1")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)
    (check (pi-model-metadata-base-url meta) => "https://mirror.example.com/coding")
    (check (pi-model-metadata-context-window meta) => 262144)
    (check (pi-model-metadata-max-tokens meta) => 32768)
  ) ;let
) ;let*

;; 5. 无 providers.json：全部内置 provider 可用
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-2.5-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,EMPTY-DIR)))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-pro")
  (let ((meta (cdr (assoc 'model-meta cfg))))
    (check (pi-model-metadata-matched? meta) => #t)
  ) ;let
) ;let*

;; 6. providers.json 语法错误时拒绝启动
(check-catch 'parse-error
  (resolve-pi-config (parse-pi-args '())
    `(("PI_CODING_AGENT_DIR" . ,(string-append FIXTURE "/bad-providers")))
  ) ;resolve-pi-config
) ;check-catch


;; --- provider 级 httpProxy ---

;; 7. provider 文件的 httpProxy 生效（覆盖 settings.json 的全局 httpProxy）
(let* ((args (parse-pi-args '("--model" "k3.1-pro")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("KIMI_API_KEY" . "kimi-key-test")
              ("HTTPS_PROXY" . "")
              ("ALL_PROXY" . ""))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'proxy-url cfg)) => "http://127.0.0.1:7890")
) ;let*

;; 8. CLI --proxy 覆盖 provider 文件的 httpProxy
(let* ((args (parse-pi-args '("--proxy" "socks5h://127.0.0.1:1080" "--provider"
                              "kimi-coding" "--model" "k3.1-pro")
             ) ;parse-pi-args
       ) ;args
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("KIMI_API_KEY" . "kimi-key-test")))
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'proxy-url cfg)) => "socks5h://127.0.0.1:1080")
) ;let*

;; 9. provider 未声明 httpProxy 时回退 settings.json 全局值
(let* ((args (parse-pi-args '("--provider" "custom-kimi" "--model" "m1")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("HTTPS_PROXY" . "")
              ("ALL_PROXY" . ""))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "custom-kimi")
  (check (cdr (assoc 'proxy-url cfg)) => "http://global-proxy:8080")
) ;let*

;; 10. 环境变量优先于 settings.json 全局值（provider 未声明时）
(let* ((args (parse-pi-args '("--provider" "custom-kimi" "--model" "m1")))
       (env `(("PI_CODING_AGENT_DIR" . ,FULL-DIR)
              ("HTTPS_PROXY" . "http://env-proxy:3128"))
       ) ;env
       (cfg (resolve-pi-config args env))
      ) ;
  (check (cdr (assoc 'proxy-url cfg)) => "http://env-proxy:3128")
) ;let*

;; --- 启动时自动激活 settings.json 中未激活的默认模型 ---

(define %test-dir-counter 0)

(define (make-test-dir fixture-name name)
  (set! %test-dir-counter (+ %test-dir-counter 1))
  (let* ((src (path-join (path FIXTURE) (path fixture-name)))
         (dst
           (path-join (path-temp-dir)
             (string-append "pi-user-catalog-test-"
               name
               "-"
               (number->string (current-second))
               "-"
               (number->string %test-dir-counter)
             ) ;string-append
           ) ;path-join
         ) ;dst
        ) ;
    (define (copy-tree s d)
      (unless (path-exists? d)
        (path-mkdir d :parents #t :exist-ok #t)
      ) ;unless
      (for-each
        (lambda (entry)
          (let ((sp (path-join s (path entry))) (dp (path-join d (path entry))))
            (if (path-stat-dir? (path-stat (path->string sp)))
              (copy-tree sp dp)
              (path-copy sp dp)
            ) ;if
          ) ;let
        ) ;lambda
        (vector->list (listdir (path->string s)))
      ) ;for-each
    ) ;define
    (copy-tree src dst)
    (path->string dst)
  ) ;let*
) ;define

;; 11. 模型级自动激活：settings 默认指向 kimi-coding/k3，被 models 数组过滤，启动自动激活且不再静默换模型
(let* ((dir (make-test-dir "unactivated-default" "model-auto-activate"))
       (args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,dir)))
       (cfg (resolve-pi-config args env))
       (p-file (path->string (path-join (path dir) (path "models") (path "kimi-coding.json")))
       ) ;p-file
       (data (string->json (path-read-text (path p-file))))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "kimi-coding")
  (check (cdr (assoc 'model-id cfg)) => "k3")
  ;; 验证 models 数组已被追加 k3
  (let ((models (json-ref data "models")))
    (check (vector? models) => #t)
    (check (vector->list models) => '("k3.1-pro" "k3"))
  ) ;let
  ;; 验证原文件其他字段与顺序未丢失
  (check (json-ref data "name") => "Kimi Coding")
  (check (json-ref data "baseUrl") => "https://api.kimi.com/coding/v1")
  (check (json-ref data "api") => "openai")
  (check (json-ref data "httpProxy") => "http://127.0.0.1:7890")
) ;let*

;; 12. provider 级自动激活：settings 默认指向 deepseek/deepseek-v4-pro，不在 providers.json 白名单，启动后自动激活
(let* ((dir (make-test-dir "unactivated-default" "prov-auto-activate"))
       (settings-file (path->string (path-join (path dir) (path "settings.json"))))
       (_ (path-write-text (path settings-file)
            "{\n  \"defaultProvider\": \"deepseek\",\n  \"defaultModel\": \"deepseek-v4-pro\"\n}\n"
          ) ;path-write-text
       ) ;_
       (args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,dir)))
       (cfg (resolve-pi-config args env))
       (pv-file (path->string (path-join (path dir) (path "models") (path "providers.json")))
       ) ;pv-file
       (pv-data (string->json (path-read-text (path pv-file))))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "deepseek")
  (check (cdr (assoc 'model-id cfg)) => "deepseek-v4-pro")
  (check (vector->list pv-data) => '("kimi-coding" "deepseek"))
) ;let*

;; 13. 默认模型在内置与模型文件均不存在时维持报错且零写入
(let* ((dir (make-test-dir "unactivated-default" "bad-model"))
       (settings-file (path->string (path-join (path dir) (path "settings.json"))))
       (_ (path-write-text (path settings-file)
            "{\n  \"defaultProvider\": \"kimi-coding\",\n  \"defaultModel\": \"completely-non-existent-model\"\n}\n"
          ) ;path-write-text
       ) ;_
       (p-file (path->string (path-join (path dir) (path "models") (path "kimi-coding.json")))
       ) ;p-file
       (before-text (path-read-text (path p-file)))
       (args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,dir)))
      ) ;
  (check-catch 'pi-config-error (resolve-pi-config args env))
  ;; 验证文件未被任何写入篡改
  (check (path-read-text (path p-file)) => before-text)
) ;let*

;; 14. 白名单文件与 models 数组缺失时全量激活，零写入
(let* ((dir (make-test-dir "no-providers" "missing-whitelist"))
       (args (parse-pi-args '()))
       (env `(("PI_CODING_AGENT_DIR" . ,dir)))
       (cfg (resolve-pi-config args env))
       (pv-file (path->string (path-join (path dir) (path "models") (path "providers.json")))
       ) ;pv-file
      ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
  (check (file-exists? pv-file) => #f)
) ;let*

(check-report)
