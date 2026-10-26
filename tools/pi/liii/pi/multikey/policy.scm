;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 密钥池轮换策略：cooldown + 固定当前 key 状态机。
;; 经 config 中的 request-key / rotate-key 钩子接入核心请求路径
;; （见 (liii pi multikey config) 与 (liii pi ai keyring)）。

(define-library (liii pi multikey policy)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi ai time)
    (liii pi ai retry)
    (liii pi multikey pool)
  ) ;import
  (export pi-multikey-select-key pi-multikey-mark-cooldown! pi-multikey-pool-of
    pi-multikey-request-key pi-multikey-rotate!
  ) ;export
  (begin

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

    ;; ---- config 钩子接入 ----

    (define (pi-multikey-pool-of config)
      (cdr (or (assoc 'multikey-pool config) '(#f . #f)))
    ) ;define

    ;; 切换日志：首次选 key 不打日志，每次实际切换打一条脱敏日志；
    ;; 复用核心 retry logger 通道（TUI 经 set-pi-retry-logger! 重定向为界面卡片）。
    (define (%multikey-log! provider status pk port)
      (pi-ai-log!
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
        port
      ) ;pi-ai-log!
    ) ;define

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
