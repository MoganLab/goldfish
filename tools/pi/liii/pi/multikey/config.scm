;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; multikey 扩展的 config augmenter：命中 <config-dir>/multikey.json 的
;; pool 时向 config 注入 api-key / multikey-pool / request-key / rotate-key，
;; 核心请求路径经 (liii pi ai keyring) 通用钩子调用，不直接依赖本库。

(define-library (liii pi multikey config)
  (import (scheme base)
    (liii list)
    (liii path)
    (liii pi multikey pool)
    (liii pi multikey load)
    (liii pi multikey policy)
  ) ;import
  (export pi-multikey-augment-config)
  (begin

    ;; 命中 pool 时替换 api-key 为首个池内 key，并注入 request-key /
    ;; rotate-key 钩子供核心 (liii pi ai keyring) 调用；
    ;; 未命中（无 multikey.json 或无对应 provider 的 pool）时原样返回。
    (define (pi-multikey-augment-config cfg . opt-parsed)
      (let* ((config-dir
               (if (pair? cfg)
                 (cdr (or (assoc 'config-dir cfg) (assoc 'agent-dir cfg) '(#f
                                                                           . #f)))
                 #f
               ) ;if
             ) ;config-dir
             (provider
               (if (pair? cfg) (cdr (or (assoc 'provider cfg) '(#f . #f))) #f)
             ) ;provider
             (pool (and (or (string? config-dir) (path? config-dir))
                     provider
                     (pi-load-multikey config-dir provider)
                   ) ;and
             ) ;pool
            ) ;
        (if (not pool)
          cfg
          (let* ((first-key (pi-pool-key-key (car (pi-key-pool-keys pool))))
                 (kept
                   (filter
                     (lambda (p) (not (memq (car p) '(api-key multikey-pool))))
                     (if (pair? cfg) cfg '())
                   ) ;filter
                 ) ;kept
                ) ;
            (cons (cons 'api-key first-key)
              (cons (cons 'multikey-pool pool)
                (cons (cons 'request-key pi-multikey-request-key)
                  (cons (cons 'rotate-key pi-multikey-rotate!) kept)
                ) ;cons
              ) ;cons
            ) ;cons
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
