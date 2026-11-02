;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; `gf pi config` 的写盘适配层：
;; 底层序列化与原子落盘由 (liii pi code config json-write) 提供，
;; 本模块兼容 <pi-config-state> 对象参数并向 modes 层导出。
(define-library (liii pi code modes config json-write)
  (import (scheme base)
    (liii pi code config json-write)
    (liii pi code modes config state)
  ) ;import
  (export pi-config-json-pretty pi-config-apply-writes)
  (begin

    (define (pi-config-apply-writes st writes)
      (let ((dir (if (string? st) st (pi-config-state-dir st))))
        (pi-apply-writes dir writes)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
