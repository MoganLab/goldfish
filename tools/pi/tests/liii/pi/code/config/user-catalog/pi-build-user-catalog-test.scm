;; pi-load-providers-whitelist / pi-build-user-catalog
;; 读取 models/providers.json 白名单并构建白名单驱动的用户模型目录：
;; 白名单文件缺失或为空数组时回退全部内置 provider；
;; 每个模型的元数据 = models/<provider>/<model>.json > 内置目录（字段级补缺）。
;;
;; 语法
;; ----
;; (pi-load-providers-whitelist dir) -> list | #f
;; (pi-build-user-catalog dir) -> alist
;;
;; 参数
;; ----
;; dir : string
;; 用户配置目录，包含 models/providers.json 与 models/ 目录。
;;
;; 返回值
;; ----
;; list | alist
;; 白名单为字符串列表（未配置时 #f）；目录为 provider 到模型列表的 alist。

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii shutil)
  (liii pi ai providers registry)
  (liii pi code config user-providers)
  (liii pi code config user-catalog)
) ;import

(check-set-mode! 'report-failed)

;; pi-load-providers-whitelist
;; 读取 models/providers.json 白名单（旧位置 providers.json 兼容）：
;; 文件缺失返回 #f（回退全部内置 provider），
;; 语法错误/元素非字符串/重复 id 抛错（唯一致命文件）。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

;; 1. 正常白名单：返回字符串列表，保持文件内顺序
(let ((wl (pi-load-providers-whitelist (string-append FIXTURE "/full"))))
  (check wl => '("google" "kimi-coding" "custom-kimi"))
) ;let

;; 2. 文件缺失：返回 #f
(check (pi-load-providers-whitelist (string-append FIXTURE "/no-providers"))
  =>
  #f
) ;check

;; 2b. 空数组：返回 #f（与文件缺失一致，视为未配置）
(check (pi-load-providers-whitelist (string-append FIXTURE "/empty-providers"))
  =>
  #f
) ;check

;; 3. 语法错误：抛错
(check-catch 'parse-error
  (pi-load-providers-whitelist (string-append FIXTURE "/bad-providers"))
) ;check-catch

;; pi-build-user-catalog
;; 白名单驱动的模型目录：provider 集 = 白名单顺序；
;; 每个模型的元数据 = models/<provider>/<model>.json > 内置目录（字段级补缺）；
;; models 白名单过滤可用模型；查无的 id 告警忽略。

(define catalog-full (pi-build-user-catalog (string-append FIXTURE "/full")))

;; 4. provider 集 = 白名单顺序（google kimi-coding custom-kimi）
(check (map car catalog-full) => '("google" "kimi-coding" "custom-kimi"))

;; 5. google 无 provider 文件：内置目录全量
(let* ((entry (assoc "google" catalog-full))
       (models (cdr (assoc "models" (cdr entry))))
      ) ;
  (check (> (length models) 10) => #t)
) ;let*

;; 6. kimi-coding：models 白名单只留 k3.1-pro；元数据来自模型文件 + 内置补缺
(let* ((entry (assoc "kimi-coding" catalog-full))
       (models (cdr (assoc "models" (cdr entry))))
      ) ;
  (check (length models) => 1)
  (let ((m (car models)))
    (check (cdr (assoc "id" m)) => "k3.1-pro")
    ;; 模型文件字段
    (check (cdr (assoc "name" m)) => "Kimi K3.1 Pro")
    (check (cdr (assoc "contextWindow" m)) => 1048576)
    ;; 模型文件缺 maxTokens？不缺（写了 131072）
    (check (cdr (assoc "maxTokens" m)) => 131072)
  ) ;let
) ;let*

;; 7. custom-kimi：无内置定义，目录模型全量；api/baseUrl 来自 provider 文件
(let* ((entry (assoc "custom-kimi" catalog-full))
       (fields (cdr entry))
       (models (cdr (assoc "models" fields)))
      ) ;
  (check (cdr (assoc "baseUrl" fields)) => "https://mirror.example.com/coding")
  (check (cdr (assoc "api" fields)) => "anthropic-messages")
  (check (length models) => 1)
  (let ((m (car models)))
    (check (cdr (assoc "id" m)) => "m1")
    (check (cdr (assoc "name" m)) => "Mirror M1")
  ) ;let
) ;let*

;; 8. 无 providers.json：全部内置 provider（字典序），模型全量内置
(let ((catalog (pi-build-user-catalog (string-append FIXTURE "/no-providers"))))
  (check (map car catalog)
    =>
    '("deepseek" "google" "kimi-coding" "minimax-cn" "ollama" "opencode"
      "zai-coding-cn")
  ) ;check
) ;let

;; 8b. 空数组 providers.json：回退全部内置 provider（字典序）
(let ((catalog (pi-build-user-catalog (string-append FIXTURE "/empty-providers"))))
  (check (map car catalog)
    =>
    '("deepseek" "google" "kimi-coding" "minimax-cn" "ollama" "opencode"
      "zai-coding-cn")
  ) ;check
) ;let

;; 8c. pi-build-config-catalog：空数组与文件缺失时全部内置 provider 均标记为 enabled
(let ((config-catalog (pi-build-config-catalog (string-append FIXTURE "/empty-providers"))
      ) ;config-catalog
     ) ;
  (check (map car config-catalog)
    =>
    '("deepseek" "google" "kimi-coding" "minimax-cn" "ollama" "opencode"
      "zai-coding-cn")
  ) ;check
  (check
    (map
      (lambda (entry) (cdr (assoc "enabled" (cdr entry))))
      config-catalog
    ) ;map
    =>
    '(#t #t #t #t #t #t #t)
  ) ;check
) ;let
(let ((config-catalog (pi-build-config-catalog (string-append FIXTURE "/no-providers"))
      ) ;config-catalog
     ) ;
  (check
    (map
      (lambda (entry) (cdr (assoc "enabled" (cdr entry))))
      config-catalog
    ) ;map
    =>
    '(#t #t #t #t #t #t #t)
  ) ;check
) ;let

;; 9. 白名单路径：models/providers.json 优先，旧位置 providers.json 仍可用
(let* ((st (pi-load-providers-whitelist (string-append FIXTURE "/full"))))
  (check st => '("google" "kimi-coding" "custom-kimi"))
) ;let*
;; 路径分隔符随平台变化（Windows 为反斜杠），断言用 path-join 生成期望后缀
(check
  (string-ends? (pi-providers-whitelist-path (string-append FIXTURE "/full"))
    (path->string (path-join (path "models") (path "providers.json")))
  ) ;string-ends?
  =>
  #t
) ;check
;; 旧位置：direct 夹具只有顶层 providers.json

(define FIXTURE-DIRECT
  (if (file-exists? "tools/pi/tests/fixtures/direct")
    "tools/pi/tests/fixtures/direct"
    "tests/fixtures/direct"
  ) ;if
) ;define
(check (string-ends? (pi-providers-whitelist-path FIXTURE-DIRECT) "providers.json")
  =>
  #t
) ;check
(check (string-ends? (pi-providers-whitelist-path FIXTURE-DIRECT)
         "models/providers.json"
       ) ;string-ends?
  =>
  #f
) ;check

;; 10. api 简写归一化：openai -> openai-completions
(let* ((entry (assoc "kimi-coding" catalog-full)))
  (check (cdr (assoc "api" (cdr entry))) => "openai-completions")
) ;let*

;; 11. api 为 JSON null 时按未配置处理，不阻断 gf pi 的配置目录构建
(let* ((catalog (pi-build-user-catalog (string-append FIXTURE "/null-api")))
       (google-fields (cdr (assoc "google" catalog)))
       (custom-fields (cdr (assoc "custom-null" catalog)))
      ) ;
  ;; 内置 provider 回退内置协议
  (check (cdr (assoc "api" google-fields)) => "google-generative-ai")
  ;; 自定义 provider 没有内置协议时省略 api 字段
  (check (assoc "api" custom-fields) => #f)
) ;let*

;; 12. all-in-one 元供应商：无白名单时动态挂入 catalog，包含有效伪模型
(let ((tmp-dir (path->string (path-join (path-temp-dir) "pi-aio-catalog-test"))))
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir #t)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join tmp-dir "all-in-one.json"))
    "{\"provider\":\"all-in-one\",\"chains\":[{\"id\":\"coding-deep\",\"nodes\":[{\"provider\":\"p\",\"model\":\"m\"}]}]}"
  ) ;path-write-text
  (let* ((cat (pi-build-user-catalog tmp-dir))
         (p-ids (map car cat))
         (aio-entry (assoc "all-in-one" cat))
        ) ;
    (check-true (and (member "all-in-one" p-ids) #t))
    (check-true (pair? aio-entry))
    (let ((models (cdr (assoc "models" (cdr aio-entry)))))
      (check (length models) => 1)
      (check (cdr (assoc "id" (car models))) => "coding-deep")
    ) ;let
  ) ;let*
  (rmtree tmp-dir #t)
) ;let

(check-report)
