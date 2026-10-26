;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai multikey)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi ai time)
    (liii pi ai config types)
    (liii pi ai config record)
  ) ;import
  (export pi-mask-key pi-load-multikey pi-multikey-select-key
    pi-multikey-mark-cooldown! pi-multikey-pool-of pi-multikey-request-key
    pi-multikey-rotate! set-pi-multikey-logger!
  ) ;export
  (begin

    ;; ---- 脱敏 ----

    (define (pi-mask-key k)
      (if (and (string? k) (> (string-length k) 10))
        (string-append (substring k 0 6)
          "…"
          (substring k (- (string-length k) 4) (string-length k))
        ) ;string-append
        "***"
      ) ;if
    ) ;define

    ;; ---- 配置读取 ----

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%parse-pool-key item)
      (and (pair? item)
        (let ((k (%dict-ref item "key" #f))
              (label (%dict-ref item "label" ""))
              (enabled (%dict-ref item "enabled" #t))
             ) ;
          (and (string? k)
            (not (string-null? k))
            (not (or (equal? enabled #f) (equal? enabled 'false) (equal? enabled "false")))
            (pi-pool-key k (if (string? label) label "") #t)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%parse-pool pool-raw provider-str)
      (and (pair? pool-raw)
        (equal? (%dict-ref pool-raw "id" #f) provider-str)
        (let* ((keys-raw (%dict-ref pool-raw "keys" '()))
               (keys (cond ((vector? keys-raw) (vector->list keys-raw))
                           ((pair? keys-raw) keys-raw)
                           (else '())
                     ) ;cond
               ) ;keys
               (parsed (filter (lambda (pk) pk) (map %parse-pool-key keys)))
               (cooldown (%dict-ref pool-raw "cooldownMs" 20000))
               (invalid-cooldown (%dict-ref pool-raw "invalidKeyCooldownMs" 600000))
              ) ;
          (and (pair? parsed)
            (pi-key-pool provider-str
              (if (number? cooldown) cooldown 20000)
              (if (number? invalid-cooldown) invalid-cooldown 600000)
              parsed
            ) ;pi-key-pool
          ) ;and
        ) ;let*
      ) ;and
    ) ;define

    (define (pi-load-multikey agent-dir provider)
      (let* ((provider-str (if (symbol? provider) (symbol->string provider) provider))
             (filepath (path->string (path-join (path agent-dir) (path "multikey.json"))))
             (data
               (and (path-exists? (path filepath))
                 (catch #t
                   (lambda () (string->json (path-read-text (path filepath))))
                   (lambda args #f)
                 ) ;catch
               ) ;and
             ) ;data
             (pools (and (pair? data) (%dict-ref data "pools" '())))
             (pool-list (cond ((vector? pools) (vector->list pools))
                              ((pair? pools) pools)
                              (else '())
                        ) ;cond
             ) ;pool-list
            ) ;
        (find (lambda (pool) pool)
          (map (lambda (raw) (%parse-pool raw provider-str)) pool-list)
        ) ;find
      ) ;let*
    ) ;define

    ;; ---- cooldown + 固定当前 key 状态机 ----

    (define (pi-multikey-mark-cooldown! pool-key reason now-ms pool)
      (let* ((invalid? (or (equal? reason 403)
                         (equal? reason "403")
                         (equal? reason 'invalid)
                         (equal? reason "invalid")
                       ) ;or
             ) ;invalid?
             (ms (if invalid?
                   (pi-key-pool-invalid-cooldown-ms pool)
                   (pi-key-pool-cooldown-ms pool)
                 ) ;if
             ) ;ms
            ) ;
        (pi-pool-key-set-cooldown-until! pool-key (+ now-ms ms))
      ) ;let*
    ) ;define

    ;; 正常请求优先复用最近使用的可用 key，保持缓存亲和性；
    ;; 当前 key 冷却后才选择下一个，初始 last-used 相同时保持配置顺序。
    (define (pi-multikey-select-key pool now-ms)
      (let loop
        ((rest (pi-key-pool-keys pool)) (best #f))
        (if (null? rest)
          (begin
            (when best
              (pi-pool-key-set-last-used! best now-ms)
            ) ;when
            best
          ) ;begin
          (let ((k (car rest)))
            (if (pi-pool-key-cooling? k now-ms)
              (loop (cdr rest) best)
              (if (or (not best) (> (pi-pool-key-last-used k) (pi-pool-key-last-used best)))
                (loop (cdr rest) k)
                (loop (cdr rest) best)
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ---- provider 层接入 ----

    (define (pi-multikey-pool-of config)
      (cdr (or (assoc 'multikey-pool config) '(#f . #f)))
    ) ;define

    (define *pi-multikey-logger* #f)

    (define (set-pi-multikey-logger! logger)
      (set! *pi-multikey-logger* logger)
    ) ;define

    (define (%multikey-log! provider status pk port)
      (let ((msg
              (string-append "multikey["
                provider
                "]: "
                (if (number? status) (number->string status) (object->string status))
                " on "
                (let ((lb (pi-pool-key-label pk)))
                  (if (and (string? lb) (not (string-null? lb))) lb "key")
                ) ;let
                " ("
                (pi-mask-key (pi-pool-key-key pk))
                "), rotating to another key"
              ) ;string-append
            ) ;msg
           ) ;
        (if (procedure? *pi-multikey-logger*)
          (*pi-multikey-logger* msg)
          (let ((p (or port (if (defined? '*stderr*) *stderr* (current-error-port)))))
            (display (string-append msg "\n") p)
            (flush-output-port p)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 切换日志：首次选 key 不打日志，每次实际切换打一条脱敏日志；
    ;; 默认写 stderr，TUI 经 set-pi-multikey-logger! 重定向为界面卡片。

    ;; 每次请求开始时调用：优先复用最近使用的可用 key，
    ;; 无 pool 时回退 config 的 api-key（auth.json 路径）。
    (define (pi-multikey-request-key config)
      (let ((pool (pi-multikey-pool-of config)))
        (if pool
          (let ((sel (pi-multikey-select-key pool (pi-current-time-millis))))
            (or (and sel (pi-pool-key-key sel))
              (cdr (or (assoc 'api-key config) '(#f . #f)))
            ) ;or
          ) ;let
          (cdr (or (assoc 'api-key config) '(#f . #f)))
        ) ;if
      ) ;let
    ) ;define

    ;; 429/403 后冷却当前 key 并返回下一可用 key；pool 耗尽返回 'exhausted。
    (define (%find-pool-key pool key)
      (find (lambda (k) (equal? (pi-pool-key-key k) key)) (pi-key-pool-keys pool))
    ) ;define

    (define* (pi-multikey-rotate! config current-key status (opt-port #f))
      (let ((pool (pi-multikey-pool-of config)))
        (if (not pool)
          #f
          (let ((now (pi-current-time-millis)))
            (let ((pk (%find-pool-key pool current-key)))
              (when pk
                (pi-multikey-mark-cooldown! pk status now pool)
                (%multikey-log! (pi-key-pool-id pool) status pk opt-port)
              ) ;when
            ) ;let
            (let ((next (pi-multikey-select-key pool now)))
              (if next (pi-pool-key-key next) 'exhausted)
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
