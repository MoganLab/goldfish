(import (scheme base)
  (liii check)
  (liii list)
  (liii pi code modes config state)
) ;import

(check-set-mode! 'report-failed)

;; pi-config-state：把用户配置目录装配成只读视图模型。
;; provider 顺序 = providers.json 白名单顺序；
;; 每个 provider 带 name/baseUrl/api/proxy/key-source/default?/models；
;; 每个 model 带 id/name/contextWindow/maxTokens/reasoning/file/default?。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"))

(define DIR (string-append FIXTURE "/config-tui"))

;; 空环境变量：key 只可能来自 auth.json
(define st (pi-config-state DIR :env '()))

(define (prov id) (pi-config-provider st id))

;; 1. provider 全量视图：白名单顺序在前，其余内置 provider 灰显在后
(check (map pi-config-provider-id (pi-config-state-providers st))
  => '("google" "kimi-coding" "custom-kimi" "deepseek" "minimax-cn" "zai-coding-cn"))

;; 1b. 可用标记：白名单内 #t，否则 #f
(check (pi-config-provider-enabled? (prov "google")) => #t)
(check (pi-config-provider-enabled? (prov "deepseek")) => #f)
(check (pi-config-state-enabled-provider-count st) => 3)

;; 2. provider 层字段：内置注册表兜底 + provider 文件覆盖
(check (pi-config-provider-base-url (prov "kimi-coding"))
  => "https://api.kimi.com/coding/v1")
(check (pi-config-provider-api (prov "kimi-coding")) => "openai-completions")
(check (pi-config-provider-name (prov "kimi-coding")) => "Kimi Coding")
(check (pi-config-provider-base-url (prov "custom-kimi"))
  => "https://mirror.example.com/coding")
;; api 简写归一化：anthropic -> anthropic-messages
(check (pi-config-provider-api (prov "custom-kimi")) => "anthropic-messages")

;; 3. key 来源：只报来源不报值
(check (pi-config-provider-key-source (prov "kimi-coding")) => "auth.json")
(check (pi-config-provider-key-source (prov "google")) => #f)

;; 4. 代理优先级：provider 文件 > settings.json 全局
(check (pi-config-provider-proxy (prov "custom-kimi")) => "http://provider-proxy:1080")
(check (pi-config-provider-proxy (prov "kimi-coding")) => "http://global-proxy:8080")

;; 5. 默认模型标记
(check (pi-config-state-default-provider st) => "kimi-coding")
(check (pi-config-state-default-model st) => "k3.1-pro")
(check (pi-config-provider-default? (prov "kimi-coding")) => #t)
(check (pi-config-provider-default? (prov "google")) => #f)

;; 6. 模型全量视图：可用（白名单内）在前，被过滤的在后
(let ((ms (pi-config-provider-models (prov "kimi-coding"))))
  (check (pi-config-model-id (car ms)) => "k3.1-pro")
  (check (pi-config-model-enabled? (car ms)) => #t)
  ;; 白名单外仍在内置目录里的模型保留但标记为不可用
  (check (length ms) => 5)
  (check (length (filter (lambda (m) (pi-config-model-enabled? m)) ms)) => 1)
) ;let

;; 7. 模型文件覆盖内置/默认，file 记录来源
(let ((m (car (pi-config-provider-models (prov "kimi-coding")))))
  (check (pi-config-model-name m) => "Kimi K3.1 Pro")
  (check (pi-config-model-context m) => 1048576)
  (check (pi-config-model-max-tokens m) => 131072)
  (check (pi-config-model-file m) => "models/kimi-coding/k3.1-pro.json")
  (check (pi-config-model-default? m) => #t))

;; 8. 无模型文件的 provider（google）：内置目录全量，file 为 #f
(let ((ms (pi-config-provider-models (prov "google"))))
  (check (> (length ms) 0) => #t)
  (check (pi-config-model-file (car ms)) => #f)
  (check (pi-config-model-default? (car ms)) => #f))

;; 9. 自定义 provider（无内置定义）：目录模型来自模型文件
(let ((m (car (pi-config-provider-models (prov "custom-kimi")))))
  (check (pi-config-model-id m) => "m1")
  (check (pi-config-model-name m) => "Mirror One")
  (check (pi-config-model-reasoning? m) => #t))

;; 10. 环境变量 key 优先于 auth.json，来源显示变量名
(let* ((st-env (pi-config-state DIR :env '(("KIMI_API_KEY" . "sk-from-env"))))
       (p (pi-config-provider st-env "kimi-coding"))
      ) ;
  (check (pi-config-provider-key-source p) => "$KIMI_API_KEY")
)

;; 11. 计数与目录
(check (pi-config-state-provider-count st) => 6)
(check (pi-config-state-enabled-provider-count st) => 3)
(check (> (pi-config-state-model-count st) 40) => #t)
;; 可用模型数 = google 全部（无 models 白名单）+ kimi 1 + custom 1
(check (pi-config-state-enabled-model-count st)
  => (+ 2 (length (pi-config-provider-models (prov "google")))))
(check (pi-config-state-dir st) => DIR)

;; 12. 第三列详情行：标签 + 值成对，不含密钥值
(let ((m (car (pi-config-provider-models (prov "kimi-coding")))))
  (let* ((rows (pi-config-model-detail-rows m (prov "kimi-coding")))
         (flat (map (lambda (row) (car row)) rows))
        ) ;
    (check (and (member "id" flat) #t) => #t)
    (check (and (member "api" flat) #t) => #t)
    (check (and (member "key" flat) #t) => #t)
    ;; 详情里不能出现 key 原文
    (check (any (lambda (row) (string=? (cadr row) "sk-kimi-test")) rows) => #f)
  ) ;let*
) ;let

;; 13. 配置目录缺失时不抛错，返回空 provider 列表
(let ((empty (pi-config-state (string-append FIXTURE "/no-providers") :env '())))
  (check (pair? (pi-config-state-providers empty)) => #t)
) ;let

(check-report)
