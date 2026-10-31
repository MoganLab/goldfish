;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 服务商用量上限（quota / usage limit）识别
;;
;; 分层约定：
;;   - 拿到 HTTP 状态码与响应 JSON 之后，**具体解析由各 provider 注册的函数完成**
;;     （见 providers/<id>.scm 的 PI_*_QUOTA_ERROR，经 (liii pi ai providers registry)
;;     取用）。各家字段结构不同：智谱看 error.code，kimi 看 error.type …
;;   - 本模块只做调度与面向用户的消息拼装，不做任何 JSON 解析，也不做文本匹配
;;     （错误消息文案会变、会被本地化，不能用来判定）。
;;   - 未注册处理函数的 provider 不做额度判定，仍按状态码走原有可重试逻辑。
;;
;; 判定结果类型为 <pi-quota-limit>（kind / code / message，定义在
;; (liii pi ai providers quota)）。
;;
;; 消息约定：`provider: 服务商官方消息原文`。

(define-library (liii pi ai limits)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi ai providers quota)
    (liii pi ai providers registry)
  ) ;import
  (export pi-detect-usage-limit pi-usage-limit-error?
    pi-usage-limit-error-message pi-usage-limit-kind
  ) ;export
  (begin

    ;; 从响应体识别「服务商额度耗尽」：完全委托给该 provider 注册的处理函数。
    ;;
    ;; 返回 <pi-quota-limit>；provider 未提供官方消息时用 HTTP 状态码兜底。
    (define* (pi-detect-usage-limit status-code body-str (provider #f))
      (let ((limit
              (and (string? body-str)
                (pi-provider-quota-error status-code
                  body-str
                  (cond ((string? provider) provider)
                        ((symbol? provider) (symbol->string provider))
                        (else #f)
                  ) ;cond
                ) ;pi-provider-quota-error
              ) ;and
            ) ;limit
           ) ;
        (if (pi-quota-limit? limit)
          (if (pi-quota-limit-message limit)
            limit
            (%make-pi-quota-limit (pi-quota-limit-kind limit)
              (pi-quota-limit-code limit)
              (if (number? status-code)
                (string-append "HTTP " (number->string status-code))
                "usage limit reached"
              ) ;if
            ) ;%make-pi-quota-limit
          ) ;if
          #f
        ) ;if
      ) ;let
    ) ;define*

    ;; HTTP 状态码 + 响应体判定是否属于「额度耗尽」（不可重试）
    (define* (pi-usage-limit-error? status-code body-str (provider #f))
      (pi-detect-usage-limit status-code body-str provider)
    ) ;define*

    (define (pi-usage-limit-kind limit)
      (and (pi-quota-limit? limit) (pi-quota-limit-kind limit))
    ) ;define

    ;; 面向用户的消息：provider 前缀 + 服务商官方消息原文（不改写、不追加建议）
    (define* (pi-usage-limit-error-message limit (provider #f))
      (if (not (pi-quota-limit? limit))
        #f
        (let* ((raw (pi-quota-limit-message limit))
               (body
                 (if (and (string? raw) (not (string-null? raw))) raw "usage limit reached")
               ) ;body
               (prefix
                 (if (and (string? provider) (not (string-null? provider)))
                   (string-append provider ": ")
                   ""
                 ) ;if
               ) ;prefix
              ) ;
          (string-append prefix body)
        ) ;let*
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
