;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 服务商用量上限（quota / usage limit）识别
;;
;; 背景：部分服务商（如 Z.AI 智谱 Coding Plan）在额度耗尽时返回 HTTP 429，
;; 与"瞬时限流"共用同一状态码。若一律按 429 重试，会白白等待退避时间并
;; 最终给出无意义的 "HTTP 429 stream error"。
;;
;; 这里按响应体内容区分两类情形：
;;   - five-hour：5 小时滚动窗口额度耗尽（短周期，等待数小时可恢复）
;;   - weekly   ：每周/每月额度耗尽（长周期，需要换号或等到下个周期）
;;
;; 识别到用量上限后，调用方应停止重试并把额度类型与重置时间直接呈现给用户。

(define-library (liii pi ai limits)
  (import (scheme base) (scheme char) (liii string) (liii json))
  (export pi-detect-usage-limit pi-usage-limit-error?
    pi-usage-limit-error-message pi-usage-limit-kind pi-usage-limit-reset
  ) ;export
  (begin

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

    ;; JSON 对象取值：兼容字符串/符号键
    (define (%val key obj default-val)
      (if (not (%alist? obj))
        default-val
        (let ((p
                (or (assoc key obj)
                  (assoc (if (symbol? key) (string->symbol key) (string->symbol key)) obj)
                ) ;or
              ) ;p
             ) ;
          (if (pair? p) (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    (define (%json-error-fields body-str)
      ;; 返回 (code . message) 形式的 alist，均可能为 #f
      (let ((parsed (catch #t (lambda () (string->json body-str)) (lambda args #f))))
        (if (not (%alist? parsed))
          ;; 非 JSON 响应：直接以原文作为错误消息
          (cons #f body-str)
          (let ((err (%val "error" parsed #f)))
            (cond ((pair? err) (cons (%val "code" err #f) (%val "message" err #f)))
                  ((string? err) (cons #f err))
                  (else (cons #f #f))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%as-string x)
      (cond ((string? x) x)
            ((number? x) (number->string x))
            ((symbol? x) (symbol->string x))
            (else #f)
      ) ;cond
    ) ;define

    (define (%contains-any? haystack needles)
      (and (string? haystack)
        (let loop
          ((ns needles))
          (cond ((null? ns) #f)
                ((string-contains haystack (car ns)) #t)
                (else (loop (cdr ns)))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; 用量上限关键词（中文 + 英文）
    (define %limit-markers
      '("上限" "限额" "配额" "超出" "余额" "usage limit" "quota"
        "exhausted")
    ) ;define

    ;; 瞬时限流关键词：这类 429 应当重试，而不是判定为额度耗尽
    (define %transient-markers
      '("rate limit" "rate_limit" "ratelimit" "too many requests" "per minute"
        "per second" "per hour" "请求过于频繁" "访问过于频繁"
        "并发" "稍后重试")
    ) ;define

    (define %five-hour-markers
      '("5小时" "5 小时" "5hour" "5 hour" "5-hour" "五小时"
        "五个小时")
    ) ;define

    (define %weekly-markers
      '("每周" "周额度" "周限额" "weekly" "week limit" "per week" "week")
    ) ;define

    (define %monthly-markers
      '("每月" "月度" "monthly" "month limit" "per month")
    ) ;define

    (define %daily-markers '("每日" "每天" "daily" "day limit" "per day"))

    (define (%digits? str)
      (and (string? str)
        (> (string-length str) 0)
        (let loop
          ((i 0))
          (cond ((>= i (string-length str)) #t)
                ((char-numeric? (string-ref str i)) (loop (+ i 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (%digits-at? str i n)
      (and (<= (+ i n) (string-length str)) (%digits? (substring str i (+ i n))))
    ) ;define

    ;; 抽取 "YYYY-MM-DD HH:MM:SS"（或 'T' 分隔）形态的重置时间
    (define (%find-reset-time msg)
      (if (not (string? msg))
        #f
        (let ((len (string-length msg)))
          (let loop
            ((i 0))
            (cond ((> (+ i 19) len) #f)
                  ((and (%digits-at? msg i 4)
                     (char=? (string-ref msg (+ i 4)) #\-)
                     (%digits-at? msg (+ i 5) 2)
                     (char=? (string-ref msg (+ i 7)) #\-)
                     (%digits-at? msg (+ i 8) 2)
                     (or (char=? (string-ref msg (+ i 10)) #\space)
                       (char=? (string-ref msg (+ i 10)) #\T)
                     ) ;or
                     (%digits-at? msg (+ i 11) 2)
                     (char=? (string-ref msg (+ i 13)) #\:)
                     (%digits-at? msg (+ i 14) 2)
                     (char=? (string-ref msg (+ i 16)) #\:)
                     (%digits-at? msg (+ i 17) 2)
                   ) ;and
                   (substring msg i (+ i 19))
                  ) ;
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 从响应体识别"服务商额度耗尽"，命中返回描述 alist，否则 #f
    ;;
    ;; 返回 alist 字段：
    ;;   kind   —— five-hour / weekly / monthly / daily / quota
    ;;   reset  —— 重置时间字符串（可能 #f）
    ;;   code   —— 服务商错误码（可能 #f）
    ;;   message—— 原始错误消息（可能 #f）
    (define (pi-detect-usage-limit body-str)
      (if (or (not (string? body-str)) (string-null? body-str))
        #f
        (let* ((fields (%json-error-fields body-str))
               (code (%as-string (car fields)))
               (msg0 (%as-string (cdr fields)))
               (msg msg0)
               (hay (string-downcase (string-append (or msg "") " " (or code ""))))
               (kind (cond
                       ;; 瞬时限流（rate limit）优先排除：它应当继续重试
                       ((%contains-any? hay %transient-markers) #f)
                       ((%contains-any? hay %five-hour-markers) "five-hour")
                       ((%contains-any? hay %weekly-markers) "weekly")
                       ((%contains-any? hay %monthly-markers) "monthly")
                       ((%contains-any? hay %daily-markers) "daily")
                       ((%contains-any? hay %limit-markers) "quota")
                       (else #f)
                     ) ;cond
               ) ;kind
              ) ;
          (if kind
            (list (cons "kind" kind)
              (cons "reset" (%find-reset-time msg))
              (cons "code" code)
              (cons "message" msg)
            ) ;list
            #f
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; HTTP 状态码 + 响应体判定是否属于"额度耗尽"（不可重试）
    (define (pi-usage-limit-error? status-code body-str)
      (and (or (= status-code 429) (= status-code 403))
        (pi-detect-usage-limit body-str)
      ) ;and
    ) ;define

    (define (pi-usage-limit-kind limit)
      (and (pair? limit) (%val "kind" limit #f))
    ) ;define

    (define (pi-usage-limit-reset limit)
      (and (pair? limit) (%val "reset" limit #f))
    ) ;define

    ;; 生成面向用户的提示信息（额度类型 + 重置时间 + 原始消息）
    (define* (pi-usage-limit-error-message limit (provider #f))
      (if (not (pair? limit))
        #f
        (let* ((kind (%val "kind" limit "quota"))
               (reset (%val "reset" limit #f))
               (raw (%val "message" limit #f))
               (label (cond ((equal? kind "five-hour") "5 小时额度已用尽（用量上限）")
                            ((equal? kind "weekly") "每周/每月额度已用尽（用量上限）")
                            ((equal? kind "monthly") "每月额度已用尽（用量上限）")
                            ((equal? kind "daily") "每日额度已用尽（用量上限）")
                            (else "用量/配额已用尽（上限）")
                      ) ;cond
               ) ;label
               (prefix
                 (if (and (string? provider) (not (string-null? provider)))
                   (string-append provider ": ")
                   ""
                 ) ;if
               ) ;prefix
               (reset-part
                 (if (and (string? reset) (not (string-null? reset)))
                   (string-append "，将于 " reset " 重置")
                   ""
                 ) ;if
               ) ;reset-part
               (raw-part
                 (if (and (string? raw) (not (string-null? raw)))
                   (string-append " [原始信息: " raw "]")
                   ""
                 ) ;if
               ) ;raw-part
              ) ;
          (string-append prefix label reset-part
            "；请等待额度重置，或切换其他模型/供应商。"
            raw-part
          ) ;string-append
        ) ;let*
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
