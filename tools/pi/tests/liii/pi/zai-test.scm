(import (liii check)
  (liii pi)
  (liii pi zai)
  (liii pi models route)
  (liii pi models meta)
) ;import

(check-set-mode! 'report-failed)

;; 1. zai-build-endpoint 端点构造测试
(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4/")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

(check (zai-build-endpoint "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions")
  =>
  "https://open.bigmodel.cn/api/coding/paas/v4/chat/completions"
) ;check

;; 2. zai-build-headers 请求头测试
(let ((headers (zai-build-headers "test-api-key")))
  (check (cdr (assoc "Authorization" headers)) => "Bearer test-api-key")
  (check (cdr (assoc "Content-Type" headers)) => "application/json")
) ;let

;; 3. provider/model 斜杠语法路由解析测试
(let-values (((p m)
              (pi-route-provider-and-model #f "zai-coding-cn/glm-5.3-flash" '() '() '())
             ) ;
            ) ;
  (check p => "zai-coding-cn")
  (check m => "glm-5.3-flash")
) ;let-values

;; 4. 纯模型 ID 自动路由测试
(let-values (((p m)
              (pi-route-provider-and-model #f "glm-5.3-flash" '() '() '())
             ) ;
            ) ;
  (check p => "zai-coding-cn")
  (check m => "glm-5.3-flash")
) ;let-values

;; 5. zai-coding-cn 元数据解析测试
(let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-5.3-flash" '())))
  (check (cdr (assoc 'id meta)) => "glm-5.3-flash")
  (check (cdr (assoc 'provider meta)) => "zai-coding-cn")
  (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/coding/paas/v4")
  (check (cdr (assoc 'max-tokens meta)) => 131072)
) ;let

(check-report)
