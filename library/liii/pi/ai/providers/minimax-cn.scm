;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai providers minimax-cn)
  (import (scheme base) (liii pi ai providers quota))
  (export PI_MINIMAX_CN_PROVIDER
    PI_MINIMAX_CN_BUILTIN_MODELS
    PI_MINIMAX_CN_QUOTA_ERROR
  ) ;export
  (begin

    ;; Provider 配置：api 为协议实现标识（对应 api/ 目录），baseUrl 为默认端点，
    ;; envKeys 为依次尝试的 API key 环境变量（对应上游 pi providers/<id>.ts 的角色）
    (define PI_MINIMAX_CN_PROVIDER
      '((api . "anthropic-messages")
        (baseUrl . "https://api.minimaxi.com/anthropic")
        (envKeys "MINIMAX_CN_API_KEY" "MINIMAX_API_KEY"))
    ) ;define

    ;; 「额度耗尽」识别：MiniMax（国内版）Token Plan 用量上限
    ;;
    ;;   HTTP 403/429 且消息尾码 = 2067（2026-10-31 真实响应，走 Anthropic 兼容
    ;;   协议，错误结构 {"type":"error","error":{"type":"rate_limit_error",
    ;;   "message":"当前已达到 Token Plan 用量上限。… (2067)"}}）。
    ;;   额度耗尽与普通瞬时限流的 error.type 同为 rate_limit_error，不能用
    ;;   type 区分；稳定判据是消息尾部的业务码。无尾码的普通限流返回 #f
    ;;   让上层继续重试。
    ;;
    ;; 返回值：kind=five-hour 的 <pi-quota-limit> 或 #f
    (define (PI_MINIMAX_CN_QUOTA_ERROR status-code body-str)
      (if (or (= status-code 429) (= status-code 403))
        (let* ((fields (pi-quota-json-fields body-str))
               (msg (cdr fields))
               (hit
                 (pi-quota-match-codes (list (or (pi-quota-message-trailing-code msg) ""))
                   '(("2067" . "five-hour"))
                 ) ;pi-quota-match-codes
               ) ;hit
              ) ;
          (if (pair? hit) (%make-pi-quota-limit (cdr hit) (car hit) msg) #f)
        ) ;let*
        #f
      ) ;if
    ) ;define

    (define PI_MINIMAX_CN_BUILTIN_MODELS
      '((("id" . "MiniMax-M3.1-Flash")
         ("name" . "MiniMax-M3.1-Flash")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 1000000)
         ("maxTokens" . 524288)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M3")
         ("name" . "MiniMax-M3")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 1048576)
         ("maxTokens" . 512000)
         ("reasoning" . #t)
         ("input" "text" "image"))
        (("id" . "MiniMax-M2.7")
         ("name" . "MiniMax-M2.7")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text"))
        (("id" . "MiniMax-M2.7-highspeed")
         ("name" . "MiniMax-M2.7-highspeed")
         ("baseUrl" . "https://api.minimaxi.com/anthropic")
         ("contextWindow" . 204800)
         ("maxTokens" . 131072)
         ("reasoning" . #t)
         ("input" "text")))
    ) ;define

  ) ;begin
) ;define-library
