;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 服务商用量上限（quota / usage limit）识别
;;
;; 判定只看结构化字段（HTTP 状态码 + 响应体业务码），不做任何文本匹配：
;; 错误消息会随服务商文案调整、也会被本地化，用它做判定既不可靠也容易误判。
;;
;; 业务码是各家服务商的私有命名空间，因此码表定义在 providers/<provider>.scm 的
;; quotaErrorCodes 字段里，经 (liii pi ai providers registry) 查询；
;; 未配置该字段的 provider 不做额度判定，仍按状态码走原有可重试逻辑。
;;
;; zai-coding-cn（智谱 / Z.AI GLM Coding Plan）的业务码语义：
;;   1302        并发槽满              -> 可重试，不是额度耗尽
;;   1303 / 1305 短时频率压力          -> 可重试，不是额度耗尽
;;   1308        5 小时额度窗口耗尽     -> 额度耗尽（不重试）
;;   1310        每周/每月额度耗尽      -> 额度耗尽（不重试）
;;   1312        模型临时拥堵          -> 可重试，不是额度耗尽

(define-library (liii pi ai limits)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii json)
    (liii pi ai providers registry)
  ) ;import
  (export pi-detect-usage-limit pi-usage-limit-error?
    pi-usage-limit-error-message pi-usage-limit-kind
  ) ;export
  (begin

    ;; provider id 归一化：大小写与首尾空白不敏感
    (define (%provider-key provider)
      (cond ((string? provider) (string-downcase (string-trim-both provider)))
            ((symbol? provider) (string-downcase (symbol->string provider)))
            (else #f)
      ) ;cond
    ) ;define

    ;; 取某个 provider 的额度耗尽码表（来自 providers/<provider>.scm 的
    ;; quotaErrorCodes 字段；未配置时为空表）
    (define (%quota-codes provider)
      (let ((key (%provider-key provider)))
        (if key (pi-provider-quota-codes key) '())
      ) ;let
    ) ;define

    ;; 判断是否为形如 ((k . v) ...) 的关联表
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

    ;; JSON 对象取值：兼容字符串键与符号键
    (define (%val key obj default-val)
      (if (not (%alist? obj))
        default-val
        (let ((p (or (assoc key obj) (assoc (string->symbol key) obj))))
          (if (pair? p) (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    ;; 业务码统一成字符串（服务商有的返回字符串，有的返回数字）
    (define (%as-code obj)
      (cond ((string? obj) (string-trim-both obj))
            ((number? obj) (number->string obj))
            ((symbol? obj) (symbol->string obj))
            (else #f)
      ) ;cond
    ) ;define

    (define (%as-message obj)
      (cond ((string? obj) obj)
            (else #f)
      ) ;cond
    ) ;define

    ;; 从响应体提取 ((候选标识 ...) . message)
    ;;
    ;; 服务商用来区分错误的字段不一致，按命中优先级收集候选标识：
    ;;   {"error":{"code":"1310","message":"..."}}                    -> error.code（智谱）
    ;;   {"code":1310,"msg":"..."}                                    -> code
    ;;   {"error":{...,"type":"exceeded_current_quota_error",...}}    -> error.type（kimi）
    ;;   {"type":"error","error":{"type":"rate_limit_error",...}}     -> type
    (define (%parse-error-fields body-str)
      (let ((parsed (catch #t (lambda () (string->json body-str)) (lambda args #f))))
        (if (not (%alist? parsed))
          (cons '() #f)
          (let* ((err (%val "error" parsed #f))
                 (err-obj (if (%alist? err) err #f))
                 (raw-candidates (list (%val "code" parsed #f)
                                   (%val "code" err-obj #f)
                                   (%val "name" err-obj #f)
                                   (%val "type" parsed #f)
                                   (%val "type" err-obj #f)
                                 ) ;list
                 ) ;raw-candidates
                 (codes
                   (let loop
                     ((rest raw-candidates) (acc '()))
                     (cond ((null? rest) (reverse acc))
                           ((%as-code (car rest)) (loop (cdr rest) (cons (%as-code (car rest)) acc)))
                           (else (loop (cdr rest) acc))
                     ) ;cond
                   ) ;let
                 ) ;codes
                 (msg
                   (%as-message (or (%val "message" parsed #f)
                                  (%val "msg" parsed #f)
                                  (%val "message" err-obj #f)
                                  (%val "msg" err-obj #f)
                                  (if (string? err) err #f)
                                ) ;or
                   ) ;%as-message
                 ) ;msg
                ) ;
            (cons codes msg)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 从响应体识别「服务商额度耗尽」，命中返回描述 alist，否则 #f
    ;;
    ;; 返回 alist 字段（字符串键）：kind / code / message
    ;;   kind    —— five-hour / weekly / quota（由 provider 码表决定）
    ;;   code    —— 命中的服务商业务码/类型（如 "1310"、"exceeded_current_quota_error"）
    ;;   message —— 服务商官方消息原文（可能 #f）
    (define* (pi-detect-usage-limit body-str (provider #f))
      (if (not (string? body-str))
        #f
        (let* ((fields (%parse-error-fields body-str))
               (codes (car fields))
               (msg (cdr fields))
               (table (%quota-codes provider))
               (hit
                 (let loop
                   ((rest codes))
                   (cond ((null? rest) #f)
                         ((assoc (car rest) table) => (lambda (entry) (cons (car rest) entry)))
                         (else (loop (cdr rest)))
                   ) ;cond
                 ) ;let
               ) ;hit
              ) ;
          (if (pair? hit)
            (list (cons "kind" (cdr (cdr hit)))
              (cons "code" (car hit))
              (cons "message" msg)
            ) ;list
            #f
          ) ;if
        ) ;let*
      ) ;if
    ) ;define*

    ;; HTTP 状态码 + 响应体判定是否属于「额度耗尽」（不可重试）
    (define* (pi-usage-limit-error? status-code body-str (provider #f))
      (if (or (= status-code 429) (= status-code 403))
        (let ((limit (pi-detect-usage-limit body-str provider)))
          (and limit (cons (cons "status" status-code) limit))
        ) ;let
        #f
      ) ;if
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
