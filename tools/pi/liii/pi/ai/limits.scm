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
;; 消息约定：`provider: 服务商官方消息原文`。

(define-library (liii pi ai limits)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii pi ai providers registry)
  ) ;import
  (export pi-detect-usage-limit pi-usage-limit-error?
    pi-usage-limit-error-message pi-usage-limit-kind
  ) ;export
  (begin

    (define (%alist? obj)
      (and (pair? obj)
        (let loop
          ((xs obj))
          (cond ((null? xs) #t)
                ((pair? (car xs)) (loop (cdr xs)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; 取 alist 中的值（兼容字符串键与符号键）
    (define (%val key alist default-val)
      (if (not (%alist? alist))
        default-val
        (let ((p (or (assoc key alist) (assoc (string->symbol key) alist))))
          (if (pair? p) (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    ;; 从响应体识别「服务商额度耗尽」：完全委托给该 provider 注册的处理函数
    ;;
    ;; 返回 alist 字段（字符串键）：kind / code / message
    ;;   kind    —— 由 provider 定义（five-hour / weekly / quota …）
    ;;   code    —— 命中的错误标识（如 "1310"、"exceeded_current_quota_error"）
    ;;   message —— 服务商官方消息原文（可能 #f）
    (define* (pi-detect-usage-limit status-code body-str (provider #f))
      (if (and (string? body-str) (string? provider))
        (pi-provider-quota-error status-code body-str provider)
        (if (and (string? body-str) (symbol? provider))
          (pi-provider-quota-error status-code body-str (symbol->string provider))
          #f
        ) ;if
      ) ;if
    ) ;define*

    ;; HTTP 状态码 + 响应体判定是否属于「额度耗尽」（不可重试）
    ;; 返回值在 provider 结果之上附加 status，供消息兜底使用
    (define* (pi-usage-limit-error? status-code body-str (provider #f))
      (let ((limit (pi-detect-usage-limit status-code body-str provider)))
        (and limit (cons (cons "status" status-code) limit))
      ) ;let
    ) ;define*

    (define (pi-usage-limit-kind limit)
      (and (pair? limit) (%val "kind" limit #f))
    ) ;define

    ;; 面向用户的消息：provider 前缀 + 服务商官方消息原文（不改写、不追加建议）
    (define* (pi-usage-limit-error-message limit (provider #f))
      (if (not (pair? limit))
        #f
        (let* ((raw (%val "message" limit #f))
               (status (%val "status" limit #f))
               (body
                 (if (and (string? raw) (not (string-null? raw)))
                   raw
                   (if (number? status)
                     (string-append "HTTP " (number->string status))
                     "usage limit reached"
                   ) ;if
                 ) ;if
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
