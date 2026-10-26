;;
;; Ported from pi-multikey (https://github.com/kslamph/multikey)
;; Original work Copyright (c) 2026 kslamph
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base) (liii pi code core extensions) (liii pi multikey config))

;; multikey 扩展：注册 config augmenter，检测 <agent-dir>/multikey.json；
;; 命中 provider 对应的密钥池时注入 request-key / rotate-key 钩子，
;; 核心请求路径经 (liii pi ai keyring) 在 429/403 时自动换 key；
;; 实现位于 (liii pi multikey pool/load/policy/config) 子库族。

(define (multikey-extension api . opt-context)
  ;; 幂等注册：重复加载时不叠加 augmenter（避免多次增强重置池状态）
  (unless (memq pi-multikey-augment-config (extension-registry-augmenters-list api))
    (register-config-augmenter! api pi-multikey-augment-config)
  ) ;unless
  "multikey-loaded"
) ;define

multikey-extension
