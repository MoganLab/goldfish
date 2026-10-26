;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 密钥池数据层：池与 key 的记录类型及脱敏工具。
;; 随插件从核心 (liii pi ai config) 迁出，仅 multikey 扩展子库使用。

(define-library (liii pi multikey pool)
  (import (scheme base))
  (export pi-mask-key pi-pool-key pi-pool-key? pi-pool-key-key pi-pool-key-label
    pi-pool-key-enabled pi-pool-key-last-used pi-pool-key-set-last-used!
    pi-pool-key-cooldown-until pi-pool-key-set-cooldown-until!
    pi-pool-key-cooling? pi-key-pool pi-key-pool? pi-key-pool-id
    pi-key-pool-cooldown-ms pi-key-pool-invalid-cooldown-ms pi-key-pool-keys
  ) ;export
  (begin

    (define-record-type <pi-pool-key>
      (%make-pi-pool-key key label enabled last-used cooldown-until)
      pi-pool-key?
      (key pi-pool-key-key)
      (label pi-pool-key-label)
      (enabled pi-pool-key-enabled)
      (last-used pi-pool-key-last-used pi-pool-key-set-last-used!)
      (cooldown-until pi-pool-key-cooldown-until pi-pool-key-set-cooldown-until!)
    ) ;define-record-type

    (define* (pi-pool-key key (label "") (enabled #t))
      (%make-pi-pool-key key label (if enabled #t #f) 0 0)
    ) ;define*

    (define (pi-pool-key-cooling? pk now-ms)
      (< now-ms (pi-pool-key-cooldown-until pk))
    ) ;define

    (define-record-type <pi-key-pool>
      (%make-pi-key-pool id cooldown-ms invalid-cooldown-ms keys)
      pi-key-pool?
      (id pi-key-pool-id)
      (cooldown-ms pi-key-pool-cooldown-ms)
      (invalid-cooldown-ms pi-key-pool-invalid-cooldown-ms)
      (keys pi-key-pool-keys)
    ) ;define-record-type

    (define* (pi-key-pool id (cooldown-ms 20000) (invalid-cooldown-ms 600000) (keys '()))
      (%make-pi-key-pool id cooldown-ms invalid-cooldown-ms keys)
    ) ;define*

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

  ) ;begin
) ;define-library
