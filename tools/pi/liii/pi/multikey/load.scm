;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; multikey.json 配置解析：读取 <agent-dir>/multikey.json 并解析出
;; 与 provider 对应的密钥池 record。

(define-library (liii pi multikey load)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi multikey pool)
  ) ;import
  (export pi-load-multikey)
  (begin

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

  ) ;begin
) ;define-library
