(import (liii check) (liii pi ai zai))

(check-set-mode! 'report-failed)

;; zai-build-endpoint
;; 构造发往智谱 BigModel Coding Copilot 端的完整 URL。
(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4/")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
       ) ;zai-build-endpoint
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

;; 缺省值
(check (zai-build-endpoint #f)
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

;; 仅主机名，无路径
(check (zai-build-endpoint "http://localhost:8080")
  =>
  "http://localhost:8080/chat/completions"
) ;check

;; 已带 /chat/completions 且带尾斜杠
(check (zai-build-endpoint "http://localhost:8080/chat/completions/")
  =>
  "http://localhost:8080/chat/completions"
) ;check

(check-report)
