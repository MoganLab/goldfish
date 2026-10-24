;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers glm)
  (import (scheme base))
  (export PI_GLM_PROVIDER)
  (begin

    ;; GLM（智谱 Anthropic 兼容端点）：无内置模型目录，模型经 models.json 配置
    (define PI_GLM_PROVIDER
      '((api . "anthropic-messages")
        (baseUrl . "https://open.bigmodel.cn/api/anthropic")
        (envKeys "ANTHROPIC_AUTH_TOKEN" "GLM_API_KEY" "ANTHROPIC_API_KEY"))
    ) ;define

  ) ;begin
) ;define-library
