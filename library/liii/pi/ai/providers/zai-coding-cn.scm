;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers zai-coding-cn)
  (import (scheme base) (liii pi ai providers quota))
  (export PI_ZAI_CODING_CN_PROVIDER
    PI_ZAI_CODING_CN_BUILTIN_MODELS
    PI_ZAI_CODING_CN_QUOTA_ERROR
  ) ;export
  (begin

    ;; Provider 配置：api 为协议实现标识（对应 api/ 目录），baseUrl 为默认端点，
    ;; envKeys 为依次尝试的 API key 环境变量（对应上游 pi providers/<id>.ts 的角色）
    (define PI_ZAI_CODING_CN_PROVIDER
      '((api . "openai-completions")
        (baseUrl . "https://open.bigmodel.cn/api/coding/paas/v4")
        (envKeys "ZAI_CODING_CN_API_KEY" "ZAI_API_KEY" "GLM_API_KEY"
          "ZHIPU_API_KEY"))
    ) ;define

    ;; 「额度耗尽」识别：智谱 / Z.AI GLM Coding Plan
    ;;
    ;;   HTTP 403/429 且 error.code ∈ {1308, 1310, 1311}
    ;;     1308 -> 5 小时额度窗口耗尽（five-hour）
    ;;     1310 -> 每周/每月额度耗尽（weekly）
    ;;     1311 -> 当前订阅套餐未开放该模型权限（model-permission）
    ;;   1302（并发槽满）/1303、1305（短时频率）/1312（模型拥堵）不是额度耗尽，
    ;;   分别返回 #f 让上层继续重试。
    ;;
    ;; 1311 与额度耗尽同属「不可重试」：套餐里没有该模型，退避重试再多次也
    ;; 不会成功（2026-11-02 实测 glm-5.2-highspeed：429 + 1311，
    ;; 原文「当前订阅套餐暂未开放GLM-5.2-Highspeed权限」）。
    ;;
    ;; 返回值：((kind . ...) (code . ...) (message . 官方原文)) 或 #f
    (define (PI_ZAI_CODING_CN_QUOTA_ERROR status-code body-str)
      (pi-quota-error-result status-code
        body-str
        '(("1308" . "five-hour")
          ("1310" . "weekly")
          ("1311" . "model-permission"))
      ) ;pi-quota-error-result
    ) ;define

    (define PI_ZAI_CODING_CN_BUILTIN_MODELS
      '((("id" . "glm-4.6v")
         ("name" . "GLM-4.6V")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 128000)
         ("maxTokens" . 32768)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "glm-4.7")
         ("name" . "GLM-4.7")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5-turbo")
         ("name" . "GLM-5-Turbo")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.1")
         ("name" . "GLM-5.1")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.2")
         ("name" . "GLM-5.2")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.2-highspeed")
         ("name" . "GLM-5.2 Highspeed")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.3")
         ("name" . "GLM-5.3")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5.3-flash")
         ("name" . "GLM-5.3-Flash")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "glm-5.3-highspeed")
         ("name" . "GLM-5.3 Highspeed")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 1000000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "glm-5v-turbo")
         ("name" . "GLM-5V-Turbo")
         ("baseUrl" . "https://open.bigmodel.cn/api/coding/paas/v4")
         ("contextWindow" . 200000)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text" "image")))
    ) ;define

  ) ;begin
) ;define-library
