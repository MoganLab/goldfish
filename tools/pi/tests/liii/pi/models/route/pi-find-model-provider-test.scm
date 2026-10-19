(import (liii check)
        (liii pi models))

(check-set-mode! 'report-failed)

;; pi-find-model-provider
;; 根据模型 ID 和 models-store 检索所属 provider，支持 preferred-provider 优先与全 provider 搜索

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/default")
      "tests/fixtures/default"
      "tools/pi/tests/fixtures/default"))

(define fixture-store (pi-load-models-store FIXTURE_DIR))

;; 1. preferred-provider 精确匹配
(check (pi-find-model-provider "gemini-3.8-flash" fixture-store "google") => "google")
(check (pi-find-model-provider "gemini-2.5-pro" fixture-store "google") => "google")
(check (pi-find-model-provider "claude-3-7-sonnet-20250219" fixture-store "anthropic") => "anthropic")

;; 2. 跨 provider 自动路由（preferred-provider 未命中时）
;; 2.1 路由到 kimi-coding（k3 在内置模型或 models-store 中）
(check (pi-find-model-provider "k3" fixture-store "google") => "kimi-coding")
(check (pi-find-model-provider "kimi-for-coding" fixture-store "google") => "kimi-coding")

;; 2.2 路由到 anthropic
(check (pi-find-model-provider "claude-3-7-sonnet-20250219" fixture-store "google") => "anthropic")

;; 2.3 路由到 glm
(check (pi-find-model-provider "glm-4-plus" fixture-store "google") => "glm")

;; 2.4 路由到 deepseek
(check (pi-find-model-provider "deepseek-chat" fixture-store "google") => "deepseek")

;; 3. 全 provider 未命中返回 #f
(check (pi-find-model-provider "non-existent-model-xyz" fixture-store "google") => #f)
(check (pi-find-model-provider "non-existent-model-xyz" '() "google") => #f)

;; 4. 多 provider 同名歧义处理
(let ((ambiguous-store
       '(("google" . (("models" . #())))
         ("anthropic" . (("models" . #((("id" . "shared-dual-model"))))))
         ("glm" . (("models" . #((("id" . "shared-dual-model")))))))))
  ;; 4.1 当 preferred-provider 为 glm 时，优先命中当前 provider
  (check (pi-find-model-provider "shared-dual-model" ambiguous-store "glm") => "glm")
  ;; 4.2 当 preferred-provider 未命中（如 google）时，按已知优先级顺序取首个命中（anthropic 优于 glm）
  (check (pi-find-model-provider "shared-dual-model" ambiguous-store "google") => "anthropic")
  ;; 4.3 未提供 preferred-provider 时，按已知优先级顺序取首个命中
  (check (pi-find-model-provider "shared-dual-model" ambiguous-store) => "anthropic"))

(check-report)
